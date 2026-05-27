module Main (main) where

import BtzsCharts.Types
import BtzsCharts.HDCurveFitting
import BtzsCharts.PlotCurves
import BtzsCharts.CLI
import Data.ByteString.Lazy as BL
import Data.Aeson
import System.Exit (exitFailure)
import Control.Monad (when)

load :: FromJSON a => FilePath -> String -> IO a
load path errorMessage = do
  byteString <- BL.readFile path
  let maybeValue = decode byteString
  case maybeValue of
    Just v -> return v
    Nothing -> do
      putStrLn $ "Error: " ++ errorMessage
      exitFailure


main :: IO ()
main = do
  opts <- parseBtzsOptions
  
  stepTablet <- load (optStepTablet opts) "Failed to load StepTablet." :: IO StepTablet
  _ <- load (optConfig opts) "Failed to load Process Configuration." :: IO ProcessConfiguration

  -- Handle Film Test
  case optFilm opts of
    Just filmPath -> do
      putStrLn $ "Processing Film Test: " ++ filmPath
      material <- load filmPath "Failed to load Film Material Test" :: IO MaterialTest
      let hdCurves = fitHDCurves stepTablet material
      let plot = plotHDCurves hdCurves
      saveToFile plot "plot.svg"
      putStrLn "Generated plot.svg"
    Nothing -> return ()

  -- Handle Paper Test
  case optPaper opts of
    Just paperPath -> do
      putStrLn $ "Processing Paper Test: " ++ paperPath
      -- Placeholder: actual paper analysis logic will go here
      putStrLn "Paper analysis not yet implemented in CLI."
    Nothing -> return ()

  when (optFilm opts == Nothing && optPaper opts == Nothing) $
    putStrLn "No film or paper test provided. Use --help for usage."
