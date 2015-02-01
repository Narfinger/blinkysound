import Sound.Pulse.Simple
import Numeric.FFT.Vector.Unitary
import qualified Data.Vector as V
import System.Hardware.Serialport


import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Control.Monad.Trans  (liftIO)
import Data.Complex
import Data.Word

serialport = "/dev/ttyACM0"
fps = 10
sample_freq = 44100
samples = round $ sample_freq / fps
leds = 60



data Led = Led { red    :: Word8
               , green  :: Word8
               , blue   :: Word8
               } deriving (Show)

finished_signal = Led { red = 0, green = 0, blue = 255}

collect :: Int -> b -> ([b],[[b]]) -> ([b],[[b]])
collect n x (cur, list) =
  if n == length cur  then ([x], cur : list)
  else (x:cur, list)

gathern :: Int -> [a] -> [[a]]
gathern num list =
  let (head, tail) = foldr (collect num) ([],[[]]) list in
  tail
  
gather3 :: [a] -> [[a]]
gather3 = gathern 3

          -- does not quite work yet because we get differtent thing

doFFT :: [Double] -> V.Vector (Complex Double)
doFFT list =
  let vector = V.fromList list
      cvector = V.map (\x -> mkPolar x 1) vector in
  run dft cvector

processSamples :: [Double] -> [Led]
processSamples list =
  let transformed = doFFT list in
  [Led {red = 0, green = 120, blue =0} | x<-[1..leds]]


packLeds :: [Led] -> B.ByteString
packLeds list = B.pack $ concat $ map (\l -> [red l, green l, blue l]) list

writeToSerial :: SerialPort -> B.ByteString -> IO ()
writeToSerial port leds = do
  let finisheds = B.pack [0,0,255]
  flush port;
  flush port;
  send port leds;
  send port finisheds;
  flush port;
     

main :: IO ()
main = do
  s <- simpleNew Nothing "blinkysound" Record Nothing "this is blinkysound"
       (SampleSpec (F32 LittleEndian) 44100 1) Nothing Nothing;
  xs <- simpleRead s $ samples :: IO [Double];
  putStrLn $ show $ length xs;
  let colors = processSamples xs;
  let bstring = packLeds colors;
  port <- openSerial serialport defaultSerialSettings { commSpeed = CS115200 }
  writeToSerial port bstring;
  -- putStrLn $ show bla
--  putStrLn $ show bla
  closeSerial port
  simpleFree s
