{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Main
Description : Interactive data entry tool for Printalyzer Densitometer.
Copyright   : (c) Marco Minutoli, 2026

License     : BSD-3
Maintainer  : Marco Minutoli <mminutoli@gmail.com>
Stability   : experimental
Portability : POSIX

CLI tool that connects to a Printalyzer Densitometer over serial, prompts
the user for darkroom test parameters, guides step-by-step readings for
each test strip, and writes a valid MaterialTest JSON file.
-}
module Main (main) where

import BtzsCharts.Densitometer
import BtzsCharts.Types

import Control.Exception (bracket)
import Data.Aeson (eitherDecodeFileStrict, encode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Char (isSpace)
import Data.IORef
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Vector.Storable as VS
import Options.Applicative
import System.Exit (exitFailure)
import System.Hardware.Serialport
import System.IO (hFlush, stdout)
import Text.Printf (printf)
import Text.Read (readMaybe)

-- | Command line options for btzs-recorder.
data RecorderOptions = RecorderOptions
  { optStepTablet :: !FilePath
  , optPort       :: !FilePath
  , optOutput     :: !(Maybe FilePath)
  }
  deriving stock (Show)

optionsParser :: Parser RecorderOptions
optionsParser = RecorderOptions
  <$> strOption
      ( long "step-tablet"
     <> short 's'
     <> metavar "STEP_TABLET_JSON"
     <> help "Path to the step tablet definition JSON file" )
  <*> strOption
      ( long "port"
     <> short 'p'
     <> metavar "SERIAL_PORT"
     <> value "/dev/ttyACM0"
     <> showDefault
     <> help "Serial port path of Printalyzer Densitometer" )
  <*> optional (strOption
      ( long "output"
     <> short 'o'
     <> metavar "OUTPUT_JSON"
     <> help "Path to write the resulting material test JSON file" ))

optsInfo :: ParserInfo RecorderOptions
optsInfo = info (optionsParser <**> helper)
  ( fullDesc
 <> progDesc "Interactive darkroom testing data entry tool with Printalyzer Densitometer"
 <> header "btzs-recorder - Darkroom Material Test Densitometer Recorder" )

-- | Interactive text prompt helper.
prompt :: String -> IO String
prompt msg = do
  putStr msg
  hFlush stdout
  getLine

-- | Interactive prompt with a default value.
promptDefault :: String -> String -> IO String
promptDefault msg defVal = do
  printf "%s [%s]: " msg defVal
  hFlush stdout
  line <- getLine
  let trimmed = trim line
  return (if null trimmed then defVal else trimmed)

-- | Interactive prompt requiring a non-empty string.
promptRequired :: String -> IO String
promptRequired msg = do
  val <- prompt msg
  let trimmed = trim val
  if null trimmed
    then do
      putStrLn "Value cannot be empty. Please try again."
      promptRequired msg
    else return trimmed

-- | Interactive prompt reading a typed value.
promptRead :: Read a => String -> IO a
promptRead msg = do
  val <- promptRequired msg
  case readMaybe val of
    Just x -> return x
    Nothing -> do
      putStrLn "Invalid format. Please enter a valid value."
      promptRead msg

-- | Interactive prompt reading an optional typed value.
promptOptional :: Read a => String -> IO (Maybe a)
promptOptional msg = do
  val <- prompt msg
  let trimmed = trim val
  if null trimmed
    then return Nothing
    else case readMaybe trimmed of
      Just x -> return (Just x)
      Nothing -> do
        putStrLn "Invalid number format. Skipping optional field."
        return Nothing

trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace

sanitizeFilename :: String -> String
sanitizeFilename = map (\c -> if c `elem` (" /\\.:*?\"<>|+" :: String) then '_' else c)

-- | Guide user through measuring a single step on the densitometer.
readPatch :: SerialPort -> IORef BS.ByteString -> MeasurementType -> Int -> Int -> Double -> IO Double
readPatch port bufRef expectedMode stepIdx totalSteps nominalDensity = do
  printf "[Step %2d of %2d] (Step tablet nominal density: %.2f)\n" stepIdx totalSteps nominalDensity
  putStrLn "  Waiting for densitometer reading..."
  hFlush stdout
  waitForValidReading
  where
    waitForValidReading = do
      res <- readNextReading port bufRef
      case res of
        Left err -> do
          printf "  Error reading densitometer: %s. Please re-measure patch.\n" err
          waitForValidReading
        Right (DensitometerReading mType val)
          | mType /= expectedMode -> do
              printf "  WARNING: Received %s reading (%.2f D), expected %s!\n"
                (show mType) val (show expectedMode)
              putStrLn "  Please switch densitometer mode and re-measure patch."
              waitForValidReading
          | otherwise -> do
              printf "  -> Recorded: %.2f D\n\n" val
              return val

-- | Guide user through measuring an entire strip of N steps with review & correction.
measureStrip :: SerialPort -> IORef BS.ByteString -> MeasurementType -> StepTablet -> IO DensityReadings
measureStrip port bufRef expectedMode tablet = do
  putStrLn $ "Measuring " ++ show totalSteps ++ " step patches sequentially..."
  initialReadings <- mapM (\(idx, nom) -> readPatch port bufRef expectedMode idx totalSteps nom)
                          (zip [1..totalSteps] nominals)
  reviewLoop initialReadings
  where
    nominals = VS.toList (densities tablet)
    totalSteps = length nominals
    reviewLoop currentVals = do
      putStrLn "--- Strip Readings Summary ---"
      mapM_ (\(idx, (nom, val)) ->
               printf "  Step %2d (nominal %4.2f): %4.2f D\n" idx nom val)
            (zip [(1 :: Int)..] (zip nominals currentVals))
      putStrLn ""
      choice <- promptDefault "Accept strip? ([y]es / [r]edo single step / [R]edo entire strip / [c]ancel)" "y"
      case choice of
        "y" -> return (VS.fromList currentVals)
        "Y" -> return (VS.fromList currentVals)
        "r" -> do
          stepNum <- promptRead (printf "Enter step number to re-measure (1-%d): " totalSteps)
          if stepNum >= 1 && stepNum <= totalSteps
            then do
              let nom = nominals !! (stepNum - 1)
              newVal <- readPatch port bufRef expectedMode stepNum totalSteps nom
              let updated = take (stepNum - 1) currentVals ++ [newVal] ++ drop stepNum currentVals
              reviewLoop updated
            else do
              putStrLn "Invalid step number."
              reviewLoop currentVals
        "R" -> measureStrip port bufRef expectedMode tablet
        "c" -> do
          putStrLn "Strip measurement cancelled."
          return (VS.fromList currentVals)
        _   -> return (VS.fromList currentVals)

main :: IO ()
main = do
  opts <- execParser optsInfo

  -- 1. Load Step Tablet
  tabletRes <- eitherDecodeFileStrict (optStepTablet opts)
  tablet <- case tabletRes of
    Left err -> do
      putStrLn $ "Error reading step tablet JSON: " ++ err
      exitFailure
    Right t -> return t

  let numSteps = VS.length (densities tablet)
  putStrLn "=========================================================="
  putStrLn "   BTZS Darkroom Test Densitometer Recorder (Printalyzer) "
  putStrLn "=========================================================="
  printf "Step Tablet: %s (%d steps)\n\n" (T.unpack $ steptabletName tablet) numSteps

  -- 2. Select Test Type
  putStrLn "Select Material Type:"
  putStrLn "  1) Film  (Transmission readings 'T')"
  putStrLn "  2) Paper (Reflection readings 'R')"
  typeChoice <- promptDefault "Choice (1 or 2)" "1"
  let isFilm = typeChoice /= "2"

  -- 3. Prompt Test Metadata
  putStrLn "\n--- Material & Process Details ---"
  matName <- promptRequired (if isFilm then "Film Name (e.g. Arista Ultra.EDU 100): " else "Paper Name (e.g. Arista Ultra.EDU Pearl): ")
  devName <- promptRequired "Developer (e.g. D76 1+1): "
  temp    <- promptRead     "Developer Temperature in °C (e.g. 20.0): "
  mIso    <- if isFilm
               then do
                 isoVal <- promptRead "Rated ISO (e.g. 100.0): "
                 return (Just isoVal)
               else return Nothing
  expTime <- promptOptional "Exposure time in seconds (constant across strips/grades, press Enter to skip): "
  defaultLux <- if isFilm
                  then promptOptional "Sensitometer light level in lux (optional, press Enter to skip): "
                  else return Nothing

  let expectedMode = if isFilm then Transmission else Reflection
      portPath     = optPort opts

  -- 4. Open Serial Port and Acquire Curves
  putStrLn $ "\nOpening serial port: " ++ portPath ++ " at 115200 8N1..."
  bufRef <- newIORef BS.empty

  bracket (openSerial portPath defaultDensitometerSettings) closeSerial $ \serialPort -> do
    putStrLn "Connected to Printalyzer Densitometer!"
    printf "Expected measurement mode: %s\n" (show expectedMode)

    materialTest <- if isFilm
      then do
        curves <- collectFilmCurves serialPort bufRef expectedMode tablet defaultLux M.empty
        let filmData = FilmTestData
              { filmName            = T.pack matName
              , filmDeveloper       = T.pack devName
              , filmTemperature     = temp
              , filmRatedIso        = maybe 100.0 id mIso
              , filmMeasurements    = curves
              , filmLux             = defaultLux
              , filmExposureTime    = expTime
              }
        return (FilmTest filmData)
      else do
        curves <- collectPaperCurves serialPort bufRef expectedMode tablet M.empty
        let paperData = PaperTestData
              { paperName           = T.pack matName
              , paperDeveloper      = T.pack devName
              , paperTemperature    = temp
              , paperMeasurements   = curves
              , paperLux            = Nothing
              , paperExposureTime   = expTime
              }
        return (PaperTest paperData)

    -- 5. Validate Measurements
    case validateMeasurements tablet materialTest of
      Left err -> do
        putStrLn $ "\nValidation Error: " ++ err
        exitFailure
      Right () -> putStrLn "\nMeasurement validation passed: all curves match step tablet length."

    -- 6. Save JSON File
    let defaultFilename = sanitizeFilename matName ++ "_" ++ sanitizeFilename devName ++ ".json"
    outPath <- case optOutput opts of
      Just p  -> return p
      Nothing -> promptDefault "Enter output JSON path" defaultFilename

    BL.writeFile outPath (encode materialTest)
    printf "\nSuccessfully saved darkroom material test to: %s\n" outPath
    putStrLn "\nTo analyze this data, run:"
    if isFilm
      then printf "  stack run btzs-charts -- --film %s --step-tablet %s\n" outPath (optStepTablet opts)
      else printf "  stack run btzs-charts -- --paper %s --step-tablet %s\n" outPath (optStepTablet opts)

-- | Loop to record multiple development time curves for Film.
collectFilmCurves :: SerialPort -> IORef BS.ByteString -> MeasurementType -> StepTablet -> Maybe Double -> M.Map Float MeasurementSeries -> IO (M.Map Float MeasurementSeries)
collectFilmCurves port bufRef expectedMode tablet defLux acc = do
  putStrLn "\n=========================================="
  putStrLn "  Record New Film Curve (Development Time)"
  putStrLn "=========================================="
  devTime <- promptRead "Enter development time in minutes (e.g. 5.5, 8.0, 11.0): "
  stripLuxPrompt <- case defLux of
    Just d  -> promptDefault (printf "Enter light level in lux for this strip (default: %.1f)" d) (show d)
    Nothing -> prompt "Enter light level in lux for this strip (optional, press Enter to skip): "
  let stripLux = case readMaybe (trim stripLuxPrompt) of
        Just val -> Just val
        Nothing  -> defLux

  printf "\nRecording strip for development time: %.1f min\n" (devTime :: Float)
  readings <- measureStrip port bufRef expectedMode tablet
  let series = MeasurementSeries readings stripLux
      newAcc = M.insert devTime series acc

  another <- promptDefault "Record another development time curve? ([y]es / [n]o)" "y"
  if another `elem` ["y", "Y", "yes", "Yes"]
    then collectFilmCurves port bufRef expectedMode tablet defLux newAcc
    else return newAcc

-- | Loop to record multiple grade curves for Paper.
collectPaperCurves :: SerialPort -> IORef BS.ByteString -> MeasurementType -> StepTablet -> M.Map T.Text MeasurementSeries -> IO (M.Map T.Text MeasurementSeries)
collectPaperCurves port bufRef expectedMode tablet acc = do
  putStrLn "\n=========================================="
  putStrLn "  Record New Paper Curve (Grade)"
  putStrLn "=========================================="
  gradeStr <- promptRequired "Enter paper grade (e.g. 2, 00, 0, 1, 3, 4): "
  gradeLux <- promptOptional (printf "Enter easel illuminance in lux for grade %s (open beam with filter, press Enter to skip): " gradeStr)

  printf "\nRecording strip for grade: %s\n" gradeStr
  readings <- measureStrip port bufRef expectedMode tablet
  let series = MeasurementSeries readings gradeLux
      newAcc = M.insert (T.pack gradeStr) series acc

  another <- promptDefault "Record another paper grade curve? ([y]es / [n]o)" "y"
  if another `elem` ["y", "Y", "yes", "Yes"]
    then collectPaperCurves port bufRef expectedMode tablet newAcc
    else return newAcc
