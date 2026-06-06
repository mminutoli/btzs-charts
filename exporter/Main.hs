module Main (main) where

import BtzsCharts.PaperProfileExport
import BtzsCharts.Types

import Data.Aeson (eitherDecodeFileStrict, encode)
import qualified Data.ByteString.Lazy as BL
import Options.Applicative
import System.Exit (exitFailure)

-- | Command line options for the exporter.
data ExporterOptions = ExporterOptions
  { optPaperPath      :: !FilePath
  , optStepTabletPath :: !FilePath
  , optOutputPath     :: !FilePath
  }
  deriving (Show)

optionsParser :: Parser ExporterOptions
optionsParser = ExporterOptions
  <$> strOption
      ( long "paper"
     <> short 'p'
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

optsInfo :: ParserInfo ExporterOptions
optsInfo = info (optionsParser <**> helper)
  ( fullDesc
 <> progDesc "Fit photographic paper test curves and export ESP32 profile"
 <> header "btzs-exporter - BTZS Paper Profile Exporter" )

main :: IO ()
main = do
  opts <- execParser optsInfo
  tabletRes <- eitherDecodeFileStrict (optStepTabletPath opts)
  case tabletRes of
    Left err -> do
      putStrLn $ "Error parsing step tablet JSON: " ++ err
      exitFailure
    Right stepTablet -> do
      testRes <- eitherDecodeFileStrict (optPaperPath opts)
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
              BL.writeFile (optOutputPath opts) (encode profile)
              putStrLn $ "Successfully wrote paper profile to "
                      ++ optOutputPath opts
