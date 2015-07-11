module Main where

import Sound.Pulse.Simple
--import Math.FFT
--import CArray
--import Numeric.FFT.Vector.Unitary
import Numeric.FFT
import System.Hardware.Serialport

import Control.Monad (replicateM_)
import Data.Ratio
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Control.Monad.Trans  (liftIO)
import Data.Complex
import Data.Word

serialport = "/dev/ttyACM1"
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

doFFT :: [Double] -> [Complex Double] 
doFFT list = dft (map (\x -> mkPolar x 1)  list)
  
  -- let vector = V.fromList list
  --     cvector = V.map (\x -> mkPolar x 1) vector in
  -- run dft cvector

processSamples :: [Double] -> [Led]
processSamples list =
  let transformed = gathern gather_size $ doFFT list
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
  return ()   

runBlink :: Simple -> SerialPort -> IO ()
runBlink sound port = do
  xs <- simpleRead sound $ samples :: IO [Double];
  -- putStrLn $ show $ length xs;
  let colors = processSamples xs;
  let bstring = packLeds colors;
  writeToSerial port bstring;


simpletestpattern :: SerialPort -> IO ()
simpletestpattern port = do
  mapM_ (testpattern port) [1..255]

testpattern :: SerialPort -> Word8 -> IO ()
testpattern port j = do
  let baselist = concat $ map (replicate 20) [j..]
  let leds = take 60 $ [ Led i (i+25 `mod` 255) (i+50 `mod` 255)  | i <- baselist]
  let bstring = packLeds leds
  writeToSerial port bstring 


main :: IO ()
main = do
  -- s <- simpleNew Nothing "blinkysound" Record Nothing "this is blinkysound"
  --      (SampleSpec (F32 LittleEndian) 44100 1) Nothing Nothing;
  port <- openSerial serialport defaultSerialSettings { commSpeed = CS115200 }
  simpletestpattern port
  -- replicateM_ 10 (runBlink s port)
  closeSerial port
  -- simpleFree s
  putStrLn "Shutting down"
