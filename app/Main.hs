module Main (main) where

import BtzsCharts.Types
import BtzsCharts.HDCurveFitting
import BtzsCharts.PlotCurves
import Data.ByteString.Lazy as BL
import Data.Aeson
import System.Exit (exitFailure)

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
  stepTablet <- load "./data/Stauffer-21steps.json" "Failed to load StepTablet." :: IO StepTablet
  material <- load "./data/AristaUltraEdu100.json" "Failed to load Material" :: IO MaterialTest

  let hdCurves = fitHDCurves stepTablet material
  let plot = plotHDCurves hdCurves

  saveToFile plot "plot.svg"
