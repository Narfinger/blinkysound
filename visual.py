import numpy as np
import sys
from Queue import Queue
from ctypes import POINTER, c_ubyte, c_void_p, c_ulong, cast
from pulseaudio.lib_pulseaudio import *
import serial
from serial import SerialException

# edit to match your sink
SINK_NAME = 'alsa_output.pci-0000_05_04.0.analog-stereo'
SERIAL_PORT = '/dev/ttyACM0'
LED_NUMBER = 60
MAX_VALUE = 10
FPS = 20
SAMPLE_RATE = 44100


#precompute some constants we need
SAMPLE_NUMBER = SAMPLE_RATE/FPS
PADDING_MOD = LED_NUMBER - (SAMPLE_NUMBER % LED_NUMBER)
PADDING_NUMBER = (PADDING_MOD / 2, PADDING_MOD / 2 + PADDING_MOD % 2)
ROUND_DECIMAL = 2
GATHER_SIZE = (SAMPLE_NUMBER + PADDING_MOD) / LED_NUMBER
CONTROL = chr(0) + chr(0) + chr(255)


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
            print 'setting up recording using', sink_info.monitor_source_name
            print
            samplespec = pa_sample_spec()
            samplespec.channels = 1
            samplespec.format = PA_SAMPLE_U8
            samplespec.rate = self.rate

            pa_stream = pa_stream_new(context, "blinkysound", samplespec, None)
            pa_stream_set_read_callback(pa_stream,
                                        self._stream_read_cb,
                                        sink_info.index)
            pa_stream_connect_record(pa_stream,
                                     sink_info.monitor_source_name,
                                     None,
                                     0)

    def stream_read_cb(self, stream, length, index_incr):
        data = c_void_p()
        pa_stream_peek(stream, data, c_ulong(length))
        data = cast(data, POINTER(c_ubyte))
        for i in xrange(length):
            # When PA_SAMPLE_U8 is used, samples values range from 128
            # to 255 because the underlying audio data is signed but
            # it doesn't make sense to return signed peaks.
            #            self._samples.put(data[i] - 128)
            self._samples.put(data[i])
        pa_stream_drop(stream)
        




def convert_steps(array):
    maxvalue = np.max(array)
    steps = maxvalue/(254.0)
    
    it = np.nditer([array, None],
                   flags = ['external_loop', 'buffered'],
                   op_flags = [['readonly'],
                               ['writeonly', 'allocate', 'no_broadcast']])
    for x,y in it:
        # simple use 255 value between red and blue
        
        y[...] =  np.round(x / steps).astype(int)
    return it.operands[1]

def gather(array):
    padded = np.lib.pad(array, PADDING_NUMBER, 'edge')

    average = np.average(
        np.reshape(padded, (LED_NUMBER, GATHER_SIZE))
                   , axis=1)

    return average


def writeToTape(serial, array, maxvalue):
    print("Writing")
    print(array)
    data = ""
    if array[0] > 1e1:
        for x in array:
            towrite = [0, 0, 0]               #rgb
            towrite[0] = x
            towrite[1] = 60
            towrite[2] = 254 - x
                
            for x in towrite:
                capped = int(min(254,max(0,x)))
                data += chr(capped)
    else:
#        cleartape(serial)
        return

    # write control
    serial.write(data)
    serial.write(CONTROL)
    serial.flushInput()
    serial.flush()


def cleartape(serial):
    zeros = [0, 0, 0] * (LED_NUMBER )
    print("Clearing tape")
    data = ""
    for x in zeros:
        data += chr(x)
    serial.write(data)
    serial.write(CONTROL)

def main():
    print("Starting")
    monitor = AudioInterface(SINK_NAME, SAMPLE_RATE)
    print "done setup"
    simulate = False
    try:
        tape = serial.Serial(SERIAL_PORT, 115200)
    except OSError:
        simulate = True
    
    while True:
        try:
            array = np.fromiter(monitor, np.int64, SAMPLE_NUMBER*2)
            ffted = np.fft.rfft(array)[1:]   #this is kind of hacky but i don't know why it gives me +1

            #doing this in seperate arrays makes python slower
#            power = np.abs(ffted) ** 2
            value = np.abs(
                np.log10(
                    ffted ** 2))

            average = gather(value)
            colored = convert_steps(average)
            #print(average)
            if not simulate:
                try:
                    writeToTape(tape, colored, 8)
                except SerialException:
                    pass
                
        except KeyboardInterrupt:
            cleartape(tape)
            sys.exit()
            
if __name__ == '__main__':
    main()



#problem:
#frequencies still seem kind of weird
#i get both channels mirrored?
