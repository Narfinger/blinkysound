module Main where

import Sound.Pulse.Simple
import Numeric.FFT.Vector.Unitary
import qualified Data.Vector as V
import System.Hardware.Serialport

import Control.Monad (forever)
import Data.Ratio
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Control.Monad.Trans  (liftIO)
import Data.Complex
import Data.Word

serialport = "/dev/ttyACM0"
fps = 10
sample_freq = 44100
samples = sample_freq `quot` fromIntegral fps
leds = 60
gather_size = samples `quot` leds



data Led = Led { red    :: Word8
               , green  :: Word8
               , blue   :: Word8
               } deriving (Show)

createLed :: Double -> Led
createLed d = Led { red = 20, green = 120, blue = 0 }

finished_signal = Led { red = 0, green = 0, blue = 255}

collect :: Int -> b -> ([b],[[b]]) -> ([b],[[b]])
collect n x (cur, list) =
  if n-1 == length cur then ([], (x: cur) : list)
  else (x:cur, list)

-- |Gathers n elements and discards the first few elements that are not even divided| --
gathern :: Int -> [a] -> [[a]]
gathern num list =
  let (_, tail) = foldr (collect num) ([],[]) list in
  tail
  
-- | Computes the average over a list| --
avg :: [Double] -> Double
avg list =
  let n = fromIntegral $ length list
      avg = foldr1 (+) list in
   avg / n

avgList :: [[Double]] -> [Double]
avgList list = map avg list

doFFT :: [Double] -> V.Vector (Complex Double)
doFFT list =
  let vector = V.fromList list
      cvector = V.map (\x -> mkPolar x 1) vector in
  run dft cvector

processSamples :: [Double] -> [Led]
processSamples list =
  let transformed = gathern gather_size $ V.toList $ doFFT list
      doubles = (map . map) (magnitude . abs) transformed
      avgs = avgList doubles in
   map createLed avgs

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
  port <- openSerial serialport defaultSerialSettings { commSpeed = CS115200 }
  forever $ do
    xs <- simpleRead s $ samples :: IO [Double];
    putStrLn $ show $ length xs;
    let colors = processSamples xs;
    let bstring = packLeds colors;
    writeToSerial port bstring;
  -- putStrLn $ show bla
--  putStrLn $ show bla
  closeSerial port
  simpleFree s
