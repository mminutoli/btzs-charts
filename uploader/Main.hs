module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Options.Applicative
import System.Exit (exitFailure)
import System.Hardware.Serialport
import System.IO (hPutStrLn, stderr)

-- | Command line options for the uploader.
data UploaderOptions = UploaderOptions
  { optPort :: !FilePath
  , optFile :: !FilePath
  }
  deriving (Show)

optionsParser :: Parser UploaderOptions
optionsParser = UploaderOptions
  <$> strOption
      ( long "port"
     <> short 'p'
     <> metavar "PORT"
     <> help "Serial port path (e.g. /dev/ttyUSB0 or COM3)" )
  <*> strOption
      ( long "file"
     <> short 'f'
     <> metavar "FILE"
     <> help "Path to the JSON paper profile file" )

optsInfo :: ParserInfo UploaderOptions
optsInfo = info (optionsParser <**> helper)
  ( fullDesc
 <> progDesc "Upload JSON paper profile to ESP32 darkroom timer over serial"
 <> header "btzs-uploader - BTZS Paper Profile Uploader" )

main :: IO ()
main = do
  opts <- execParser optsInfo
  let filePath = optFile opts
      portPath = optPort opts

  -- Read profile file
  payload <- BS.readFile filePath
  let size = BS.length payload
      filename = reverse . takeWhile (\c -> c /= '/' && c /= '\\') . reverse $
                 filePath
      headerBS = BSC.pack $
        "[START_UPLOAD:" ++ filename ++ ":" ++ show size ++ "]"

  putStrLn $ "Opening serial port " ++ portPath ++ "..."
  let settings = defaultSerialSettings
        { timeout = 10
        , commSpeed = CS115200
        }
  bracket (openSerial portPath settings) closeSerial $ \serialPort -> do
    putStrLn "Waiting 2 seconds for ESP32 auto-reset to settle..."
    threadDelay 2000000

    putStrLn $ "Sending upload header: " ++ BSC.unpack headerBS
    _ <- send serialPort headerBS

    putStrLn "Waiting 0.5 seconds for filesystem to open..."
    threadDelay 500000

    putStrLn "Streaming payload..."
    _ <- send serialPort payload

    putStrLn "Waiting for confirmation..."
    response <- recv serialPort 100

    if BSC.pack "-[UPLOAD_OK]-" `BS.isInfixOf` response
      then putStrLn "Upload successful!"
      else do
        hPutStrLn stderr $ "Upload failed. Response: " ++ BSC.unpack response
        exitFailure
