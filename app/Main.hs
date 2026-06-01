module Main (main) where

import BtzsCharts.Types
import BtzsCharts.HDCurveFitting
import BtzsCharts.PlotCurves
import BtzsCharts.CLI
import BtzsCharts.FieldCharts (calculateCurveStats, fitFieldChartModel)
import BtzsCharts.PaperAnalysis (logExposureRange)

import Data.ByteString.Lazy as BL
import Data.Aeson
import System.Exit (exitFailure)
import Control.Monad (when)
import Control.Monad.Reader (runReader)
import Data.List (minimumBy)
import qualified Data.Text as T

load :: FromJSON a => FilePath -> String -> IO a
load path errorMessage = do
  byteString <- BL.readFile path
  let maybeValue = decode byteString
  case maybeValue of
    Just v -> return v
    Nothing -> do
      putStrLn $ "Error: " ++ errorMessage
      exitFailure

findLerForGrade :: StepTablet -> MaterialTest -> String -> ProcessConfiguration -> Double
findLerForGrade stepTablet (PaperTest d) targetGrade config =
  let paperCurves = fitHDCurves stepTablet (PaperTest d)
      targetVal = case targetGrade of
                    "00" -> -0.5
                    "0"  -> 0.0
                    s    -> case reads s of
                              [(val, "")] -> val
                              _ -> 2.0
      -- Find curve with closest developmentTime (which stores the grade value)
      bestCurve = minimumBy (\c1 c2 -> compare (abs (developmentTime c1 - targetVal)) (abs (developmentTime c2 - targetVal))) paperCurves
  in runReader (logExposureRange bestCurve) config
findLerForGrade _ (FilmTest{}) _ _ = error "Expected paper test for LER calculation"

main :: IO ()
main = do
  opts <- parseBtzsOptions

  stepTablet <- load (optStepTablet opts) "Failed to load StepTablet." :: IO StepTablet
  config <- load (optConfig opts) "Failed to load Process Configuration." :: IO ProcessConfiguration

  -- Handle Film Test
  case optFilm opts of
    Just filmPath -> do
      putStrLn $ "Processing Film Test: " ++ filmPath
      material <- load filmPath "Failed to load Film Material Test" :: IO MaterialTest
      case material of
        FilmTest filmData -> do
          let hdCurves = fitHDCurves stepTablet material

          -- Save standard HD Curves plot
          let plot = plotHDCurves hdCurves
          saveToFile plot "plot.svg"
          putStrLn "Generated plot.svg"

          -- Now load paper data for LER calculation
          let paperPath = case optPaper opts of
                            Just p -> p
                            Nothing -> "./data/MockPaperTest.json"
          putStrLn $ "Loading Paper Test for LER calculation from: " ++ paperPath
          paperMaterial <- load paperPath "Failed to load Paper Material Test" :: IO MaterialTest

          let ler = findLerForGrade stepTablet paperMaterial (optGrade opts) config
              ratedIso = filmRatedIso filmData
              sbrs = if Prelude.null (optSbrs opts) then [3.0, 4.0, 5.0] else optSbrs opts

          putStrLn $ "LER for grade " ++ optGrade opts ++ " is: " ++ show ler
          putStrLn $ "Rated ISO is: " ++ show ratedIso
          putStrLn $ "Generating field charts for measuring ranges (stops): " ++ show sbrs

          let sbrData = Prelude.map (\sbr ->
                let stats = runReader (calculateCurveStats hdCurves ratedIso ler sbr) config
                    model = fitFieldChartModel stats
                in (sbr, model, stats)) sbrs

              filmNameStr = T.unpack (filmName filmData)
              filmDevStr = T.unpack (filmDeveloper filmData)

              -- Sanitize film and developer names for filename use
              sanitize c
                | c `Prelude.elem` (" /\\.:*?\"<>|+" :: String) = '_'
                | otherwise = c
              cleanName = Prelude.map sanitize filmNameStr
              cleanDev = Prelude.map sanitize filmDevStr

              prefix = cleanName ++ "_" ++ cleanDev
              devtimeFile = prefix ++ "_devtime.svg"
              speedFile = prefix ++ "_speed.svg"

              devtimeLayout = plotFieldChartDevTime filmNameStr filmDevStr sbrData
              speedLayout = plotFieldChartSpeed filmNameStr filmDevStr sbrData

          saveToFile devtimeLayout devtimeFile
          saveToFile speedLayout speedFile
          putStrLn $ "Generated field charts: " ++ devtimeFile ++ " and " ++ speedFile

        PaperTest _ -> do
          putStrLn "Error: film file must be of type Film"
          exitFailure

    Nothing -> return ()

  -- Handle Paper Test (if run standalone)
  case optPaper opts of
    Just paperPath | optFilm opts == Nothing -> do
      putStrLn $ "Processing Paper Test: " ++ paperPath
      paperMaterial <- load paperPath "Failed to load Paper Material Test" :: IO MaterialTest
      let hdCurves = fitHDCurves stepTablet paperMaterial
      let plot = plotHDCurves hdCurves
      saveToFile plot "paper_plot.svg"
      putStrLn "Generated paper_plot.svg"
    _ -> return ()

  when (optFilm opts == Nothing && optPaper opts == Nothing) $
    putStrLn "No film or paper test provided. Use --help for usage."
