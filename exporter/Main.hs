{-# LANGUAGE DerivingStrategies #-}

module Main (main) where

import BtzsCharts.PaperProfileExport
import BtzsCharts.Types

import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Data.Aeson (eitherDecodeFileStrict, encode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import Options.Applicative
import System.Exit (exitFailure)
import System.Hardware.Serialport
import System.IO (hPutStrLn, stderr)

-- | Mode of operation for btzs-exporter.
data ExporterMode
  = ExportProfileMode !FilePath !FilePath !FilePath
  | UploadConfigMode !FilePath !FilePath
  deriving stock (Show)

optionsParser :: Parser ExporterMode
optionsParser = uploadParser <|> exportParser
  where
    uploadParser = UploadConfigMode
      <$> strOption
          ( long "upload-config"
         <> metavar "CONFIG_FILE"
         <> help "Path to process configuration JSON file to upload to the timer" )
      <*> strOption
          ( long "port"
         <> short 'p'
         <> metavar "PORT"
         <> help "Serial port path (e.g. /dev/ttyUSB0 or COM3)" )

    exportParser = ExportProfileMode
      <$> strOption
          ( long "paper"
         <> metavar "PAPER_FILE"
         <> help "Path to the paper material test JSON file" )
      <*> strOption
          ( long "step-tablet"
         <> short 's'
         <> metavar "TABLET_FILE"
         <> help "Path to the step-tablet definition JSON file" )
      <*> strOption
          ( long "output"
         <> short 'o'
         <> metavar "OUTPUT_FILE"
         <> help "Path to write the fitted JSON paper profile" )

optsInfo :: ParserInfo ExporterMode
optsInfo = info (optionsParser <**> helper)
  ( fullDesc
 <> progDesc "Fit paper curves or upload process configurations to ESP32 timer"
 <> header "btzs-exporter - BTZS Paper Profile & Process Exporter" )

-- | Upload a process configuration file to the ESP32 timer over serial.
uploadProcessConfig :: FilePath -> FilePath -> IO ()
uploadProcessConfig configPath portPath = do
  -- Validate process config JSON
  cfgRes <- eitherDecodeFileStrict configPath :: IO (Either String ProcessConfiguration)
  case cfgRes of
    Left err -> do
      hPutStrLn stderr $ "Error: Invalid process configuration JSON: " ++ err
      exitFailure
    Right cfg -> do
      putStrLn $ "Validated process configuration: " ++ T.unpack (processName cfg)
      payload <- BS.readFile configPath
      let size = BS.length payload
          filename = reverse . takeWhile (\c -> c /= '/' && c /= '\\') . reverse $ configPath
          headerBS = BSC.pack $ "[START_CONFIG_UPLOAD:" ++ filename ++ ":" ++ show size ++ "]"

      putStrLn $ "Opening serial port " ++ portPath ++ "..."
      let settings = defaultSerialSettings
            { timeout = 10
            , commSpeed = CS115200
            }
      bracket (openSerial portPath settings) closeSerial $ \serialPort -> do
        putStrLn "Waiting 2 seconds for ESP32 auto-reset to settle..."
        threadDelay 2000000
        _ <- recv serialPort 1000

        putStrLn $ "Sending upload header: " ++ BSC.unpack headerBS
        _ <- send serialPort headerBS

        putStrLn "Waiting 0.5 seconds for filesystem to open..."
        threadDelay 500000

        putStrLn "Streaming payload..."
        _ <- send serialPort payload

        putStrLn "Waiting for confirmation..."
        response <- recv serialPort 100

        if BSC.pack "-[UPLOAD_OK]-" `BS.isInfixOf` response
          then putStrLn "Process configuration upload successful!"
          else do
            hPutStrLn stderr $ "Upload failed. Response: " ++ BSC.unpack response
            exitFailure

main :: IO ()
main = do
  mode <- execParser optsInfo
  case mode of
    UploadConfigMode cfgFile port ->
      uploadProcessConfig cfgFile port
    ExportProfileMode paperPath tabletPath outPath -> do
      tabletRes <- eitherDecodeFileStrict tabletPath
      case tabletRes of
        Left err -> do
          putStrLn $ "Error parsing step tablet JSON: " ++ err
          exitFailure
        Right stepTablet -> do
          testRes <- eitherDecodeFileStrict paperPath
          case testRes of
            Left err -> do
              putStrLn $ "Error parsing paper test JSON: " ++ err
              exitFailure
            Right materialTest ->
              case materialTest of
                FilmTest _ -> do
                  putStrLn $ "Error: Provided material test is a Film test. "
                          ++ "Expected a Paper test."
                  exitFailure
                PaperTest _ -> do
                  let profile = buildTargetProfile stepTablet materialTest
                  BL.writeFile outPath (encode profile)
                  putStrLn $ "Successfully wrote paper profile to "
                          ++ outPath
