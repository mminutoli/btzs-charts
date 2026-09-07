{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : BtzsCharts.Densitometer
Description : Driver and parser for Printalyzer Densitometer serial communication.
Copyright   : (c) Marco Minutoli, 2026

License     : BSD-3
Maintainer  : Marco Minutoli <mminutoli@gmail.com>
Stability   : experimental
Portability : POSIX

Support for acquiring density readings from a Printalyzer Densitometer
connected over a serial interface.
-}
module BtzsCharts.Densitometer
  ( MeasurementType(..)
  , DensitometerReading(..)
  , parseDensitometerReading
  , readSerialLine
  , readNextReading
  , defaultDensitometerSettings
  ) where

import Control.Concurrent (threadDelay)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.IORef
import System.Hardware.Serialport
import Text.Read (readMaybe)

-- | Mode of measurement reported by the densitometer.
data MeasurementType
  = Transmission  -- ^ 'T' - Transmission mode (film)
  | Reflection    -- ^ 'R' - Reflection mode (paper)
  deriving stock (Show, Eq)

-- | A parsed reading from the densitometer.
data DensitometerReading = DensitometerReading
  { readingType  :: !MeasurementType
  , readingValue :: !Double
  }
  deriving stock (Show, Eq)

-- | Parse a raw ASCII string sent by the Printalyzer Densitometer.
-- Expected formats:
--   "T+0.04D\r\n"
--   "R+0.69D\n"
parseDensitometerReading :: String -> Either String DensitometerReading
parseDensitometerReading rawStr =
  let s = filter (`notElem` (" \t\r\n" :: String)) rawStr
  in case s of
    [] -> Left "Empty reading"
    (modeChar : rest) -> do
      mType <- case modeChar of
        'T' -> Right Transmission
        'R' -> Right Reflection
        c   -> Left $ "Unknown measurement mode: " ++ [c] ++ " (expected 'T' or 'R')"
      case reverse rest of
        ('D' : revNum) ->
          let numStr = reverse revNum
          in case numStr of
            ('+':valStr) -> case readMaybe valStr of
              Just val -> Right (DensitometerReading mType val)
              Nothing  -> Left $ "Failed to parse density value: " ++ valStr
            ('-':valStr) -> case readMaybe valStr of
              Just val -> Right (DensitometerReading mType (-val))
              Nothing  -> Left $ "Failed to parse density value: -" ++ valStr
            valStr -> case readMaybe valStr of
              Just val -> Right (DensitometerReading mType val)
              Nothing  -> Left $ "Failed to parse density value: " ++ valStr
        _ -> Left "Missing 'D' suffix in reading"

-- | Default serial port settings for Printalyzer Densitometer (115200 8N1).
defaultDensitometerSettings :: SerialPortSettings
defaultDensitometerSettings = defaultSerialSettings
  { commSpeed = CS115200
  , timeout   = 10  -- 1 second timeout for non-blocking poll
  }

-- | Read one newline-delimited line from the serial port using an IORef buffer.
readSerialLine :: SerialPort -> IORef BS.ByteString -> IO String
readSerialLine port bufRef = loop
  where
    loop = do
      buf <- readIORef bufRef
      case BSC.elemIndex '\n' buf of
        Just idx -> do
          let (lineBS, restWithNl) = BSC.splitAt idx buf
              rest = BSC.drop 1 restWithNl  -- drop '\n'
          writeIORef bufRef rest
          let lineStr = filter (/= '\r') (BSC.unpack lineBS)
          return lineStr
        Nothing -> do
          chunk <- recv port 64
          if BS.null chunk
            then do
              threadDelay 10000  -- 10ms pause before retrying
              loop
            else do
              writeIORef bufRef (buf `BS.append` chunk)
              loop

-- | Read and parse the next densitometer reading from the serial port.
readNextReading :: SerialPort -> IORef BS.ByteString -> IO (Either String DensitometerReading)
readNextReading port bufRef = do
  line <- readSerialLine port bufRef
  return (parseDensitometerReading line)
