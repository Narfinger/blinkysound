import numpy as np
import sys
from Queue import Queue
from ctypes import POINTER, c_ubyte, c_void_p, c_ulong, cast
from pulseaudio.lib_pulseaudio import *
import curses
from curses import wrapper
import serial

# edit to match your sink
SINK_NAME = 'alsa_output.pci-0000_05_04.0.analog-stereo'
SERIAL_PORT = '/dev/ttyACM0'
LED_NUMBER = 62                       #for some reason the first seems really out of wack after fft

# the frames my blinkytape can do is roughly 1 frame per second

MAX_VALUE = 200
FPS = 35
SAMPLE_RATE = 44100/FPS
GATHER_SIZE = SAMPLE_RATE/LED_NUMBER
ROUND_DECIMAL = 2
STEP_FREQUENCY = 200.0/254


class AudioInterface(object):
    
    def __init__(self, sink_name, rate):
        self.sink_name = sink_name
        self.rate = rate

        # Wrap callback methods in appropriate ctypefunc instances so
        # that the Pulseaudio C API can call them
        self._context_notify_cb = pa_context_notify_cb_t(self.context_notify_cb)
        self._sink_info_cb = pa_sink_info_cb_t(self.sink_info_cb)
        self._stream_read_cb = pa_stream_request_cb_t(self.stream_read_cb)

        # stream_read_cb() puts peak samples into this Queue instance
        self._samples = Queue()

        # Create the mainloop thread and set our context_notify_cb
        # method to be called when there's updates relating to the
        # connection to Pulseaudio
        _mainloop = pa_threaded_mainloop_new()
        _mainloop_api = pa_threaded_mainloop_get_api(_mainloop)
        context = pa_context_new(_mainloop_api, '')
        pa_context_set_state_callback(context, self._context_notify_cb, None)
        pa_context_connect(context, None, 0, None)
        pa_threaded_mainloop_start(_mainloop)
        
    def __iter__(self):
        while True:
            yield self._samples.get()

    def context_notify_cb(self, context, _):
        state = pa_context_get_state(context)

        if state == PA_CONTEXT_READY:
            print "Pulseaudio connection ready..."
            # Connected to Pulseaudio. Now request that sink_info_cb
            # be called with information about the available sinks.
            o = pa_context_get_sink_info_list(context, self._sink_info_cb, None)
            pa_operation_unref(o)

        elif state == PA_CONTEXT_FAILED :
            print "Connection failed"

        elif state == PA_CONTEXT_TERMINATED:
            print "Connection terminated"

    def sink_info_cb(self, context, sink_info_p, _, __):
        "gives us information about the sinks found"
        if not sink_info_p:
            return

        sink_info = sink_info_p.contents
        print '-'* 60
        print 'index:', sink_info.index
        print 'name:', sink_info.name
        print 'description:', sink_info.description

        if sink_info.name == self.sink_name:
            # Found the sink we want to monitor for peak levels.
            # Tell PA to call stream_read_cb with peak samples.
            print
            print 'setting up peak recording using', sink_info.monitor_source_name
            print
            samplespec = pa_sample_spec()
            samplespec.channels = 1
            samplespec.format = PA_SAMPLE_U8
            samplespec.rate = self.rate

            pa_stream = pa_stream_new(context, "peak detect demo", samplespec, None)
            pa_stream_set_read_callback(pa_stream,
                                        self._stream_read_cb,
                                        sink_info.index)
            pa_stream_connect_record(pa_stream,
                                     sink_info.monitor_source_name,
                                     None,
                                     PA_STREAM_PEAK_DETECT)

    def stream_read_cb(self, stream, length, index_incr):
        data = c_void_p()
        pa_stream_peek(stream, data, c_ulong(length))
        data = cast(data, POINTER(c_ubyte))
        for i in xrange(length):
            # When PA_SAMPLE_U8 is used, samples values range from 128
            # to 255 because the underlying audio data is signed but
            # it doesn't make sense to return signed peaks.
            self._samples.put(data[i] - 128)
        pa_stream_drop(stream)
        




def convert_steps(array):
    it = np.nditer([array, None],
                   flags = ['external_loop', 'buffered'],
                   op_flags = [['readonly'],
                               ['writeonly', 'allocate', 'no_broadcast']])
    for x,y in it:
        # simple use 255 value between red and blue
        y[...] =  np.round(x / STEP_FREQUENCY).astype(int)
    return it.operands[1]

def gather(array):
    length = len(array)
    reshaped = np.reshape(array, (length/GATHER_SIZE, GATHER_SIZE)) 
    average = np.average(reshaped, axis=1)
    return average


def writeToTape(serial, array):
    data = ""
    for x in array:
        towrite = [0, 0, 0]
        value = int(x)
        if value <= 66:
            towrite[0] = value
        elif value <= 133:
            towrite[1] = value
        else:
            towrite[2] = value

        for x in towrite:
            data += chr(x/3)

    # write control
    serial.write(chr(0) + chr(0) + chr(255))
    serial.write(data)
    serial.flushInput()
    serial.flush()

def main(stdscr):
    print("Starting")
    monitor = AudioInterface(SINK_NAME, SAMPLE_RATE)
    print "done setup"
    tape = serial.Serial(SERIAL_PORT, 115200)

    while True:
        array = np.fromiter(monitor, np.int64, SAMPLE_RATE)
        ffted = np.fft.fft(array)
        fftabs = np.absolute(ffted) # i guess this ranges from 0 to 1000
        average = gather(fftabs)
        modified_average = np.clip(np.round(average,ROUND_DECIMAL), 0, MAX_VALUE)[1:-2]
        colored = convert_steps(modified_average)

        writeToTape(tape, colored)

        if not stdscr:
#            print(len(modified_average))
#            print(modified_average)
            print(colored)

        else:
            for i,x in enumerate(modified_average[:30]):
                string = "%3.0f" % x
                stdscr.addstr(0,i*6,string)
            for i,x in enumerate(modified_average[30:60]):
                string = "%3.0f" % x
                stdscr.addstr(2,i*6,string)

            stdscr.refresh()

    
if __name__ == '__main__':
    # just for debugging
    if len(sys.argv)==1:
        wrapper(main)
    else:
        main(None)

#problem:
# i think i just sample to small window and don't get high frequency, perhaps take larger window and combine (mean) adjacent entries?
# there is probably theory how much i need to get all frequencies
# the high frequencies are still very noise which is to be expected
# the low frequencies are also noisy which seems weird
