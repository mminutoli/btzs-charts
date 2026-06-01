{-|
Module      : BtzsCharts.PlotCurves
Description : Plot the curves produced by the analysis.
Copyright   : (c) Marco Minutoli, 2025

License     : BSD-3
Maintainer  : Marco Minutoli <mminutoli@gmail.com>
Stability   : experimental
Portability : POSIX
-}

module BtzsCharts.PlotCurves(
    plotHDCurves,
    plotFieldChartDevTime,
    plotFieldChartSpeed,
    saveToFile
  ) where

import Control.Lens
import Data.Default.Class
import Data.Colour(opaque)
import Data.Colour.Palette.BrewerSet
import BtzsCharts.HDCurveFitting (HDCurve(..))
import BtzsCharts.FieldCharts (FieldChartModel(..), FilmCurveStats(..), evaluateTimeModel, evaluateSpeedModel)
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Backend.Diagrams
import Data.Vector.Storable(toList)
import BtzsCharts.Types (Density)
import Text.Printf (printf)

plotHDCurves :: [HDCurve] -> Layout Density Density
plotHDCurves curves = layout
  where
    hdLine i c =
      plot_lines_title .~ printf "%.1f min" (developmentTime c)
      $ plot_lines_style . line_color .~ color i
      $ plot_lines_style . line_width .~ 2.5
      $ plot_lines_values .~ [zip (toList $ relativeLogExposure c) (toList $ outputDensity c)]
      $ def
    layout = layout_title .~ "HD-Curve"
      $ layout_plots .~ Prelude.map toPlot (Prelude.zipWith hdLine [0..] curves)
      $ layout_legend .~ Just (def & legend_orientation .~ LOCols 1)
      $ layout_x_axis . laxis_generate .~ photographicAxis
      $ layout_y_axis . laxis_generate .~ photographicAxis
      $ layout_x_axis . laxis_title .~ "Relative Log Exposure"
      $ layout_y_axis . laxis_title .~ "Density"
      $ def
    color i = opaque $ brewerSet Set1 (length curves) !! i

photographicAxis :: AxisFn Density
photographicAxis xs =
  let xs' = 0.0 : 3.0 : xs
      ad = autoScaledAxis def xs'
      minVal = minimum xs'
      maxVal = maximum xs'
      start = (fromIntegral (floor (minVal * 10) :: Integer)) / 10
      end   = (fromIntegral (ceiling (maxVal * 10) :: Integer)) / 10
      allTicks = [start, start + 0.1 .. end + 0.05]

      mkTick v =
        let v10 = round (v * 10) :: Integer
            isMajor = v10 `mod` 3 == 0
        in if isMajor
           then ((v, 10), Just (printf "%.1f" v))
           else ((v, 5), Nothing)

      ticksData = Prelude.map mkTick allTicks

      adTicks = Prelude.map fst ticksData
      adLabels = [[ (v, l) | ((v, _), Just l) <- ticksData ]]
      adGrid = [ v | ((v, _), Just _) <- ticksData ]

  in ad { _axis_ticks = adTicks
        , _axis_labels = adLabels
        , _axis_grid = adGrid
        }

saveToFile :: ToRenderable p => p -> FilePath -> IO()
saveToFile layout path = do
  let opts = fo_size .~ (600, 600) $ def
  _ <- renderableToFile opts path renderable
  return ()
  where
    renderable = toRenderable layout

-- | Plot the Development Time vs N-Value for multiple SBRs on the same layout.
plotFieldChartDevTime :: String -> String -> [(Double, FieldChartModel, [FilmCurveStats])] -> Layout Double Double
plotFieldChartDevTime filmName developer sbrData = layout
  where
    plotsForSbr i (sbr, model, stats) =
      let clr = color i
          nRange = [-3.0, -2.9 .. 3.0]
          lineVals = [(n, realToFrac (evaluateTimeModel model n)) | n <- nRange]
          linePlot = plot_lines_title .~ printf "SBR %.1f stops (Fit)" sbr
                   $ plot_lines_style . line_color .~ clr
                   $ plot_lines_style . line_width .~ 2.5
                   $ plot_lines_values .~ [lineVals]
                   $ def
          pts = [(statsNValue stat, realToFrac (statsDevTime stat)) | stat <- stats]
          pointsPlot = plot_points_title .~ printf "SBR %.1f stops (Data)" sbr
                     $ plot_points_style . point_color .~ clr
                     $ plot_points_style . point_radius .~ 4.0
                     $ plot_points_style . point_shape .~ PointShapeCircle
                     $ plot_points_values .~ pts
                     $ def
      in [toPlot linePlot, toPlot pointsPlot]

    allPlots = Prelude.concat $ Prelude.zipWith plotsForSbr [0..] sbrData

    layout = layout_title .~ printf "Development Time vs N-Value - %s (%s)" filmName developer
      $ layout_plots .~ allPlots
      $ layout_legend .~ Just (def & legend_orientation .~ LOCols 1)
      $ layout_x_axis . laxis_title .~ "N-Value"
      $ layout_y_axis . laxis_title .~ "Development Time (min)"
      $ def

    color i =
      let numColors = Prelude.max 3 (Prelude.min 9 (length sbrData))
          palette = brewerSet Set1 numColors
      in opaque $ palette !! (i `mod` length palette)

-- | Plot the Effective ISO Speed vs N-Value for multiple SBRs on the same layout.
plotFieldChartSpeed :: String -> String -> [(Double, FieldChartModel, [FilmCurveStats])] -> Layout Double Double
plotFieldChartSpeed filmName developer sbrData = layout
  where
    plotsForSbr i (sbr, model, stats) =
      let clr = color i
          nRange = [-3.0, -2.9 .. 3.0]
          lineVals = [(n, evaluateSpeedModel model n) | n <- nRange]
          linePlot = plot_lines_title .~ printf "SBR %.1f stops (Fit)" sbr
                   $ plot_lines_style . line_color .~ clr
                   $ plot_lines_style . line_width .~ 2.5
                   $ plot_lines_values .~ [lineVals]
                   $ def
          pts = [(statsNValue stat, statsIsoSpeed stat) | stat <- stats]
          pointsPlot = plot_points_title .~ printf "SBR %.1f stops (Data)" sbr
                     $ plot_points_style . point_color .~ clr
                     $ plot_points_style . point_radius .~ 4.0
                     $ plot_points_style . point_shape .~ PointShapeCircle
                     $ plot_points_values .~ pts
                     $ def
      in [toPlot linePlot, toPlot pointsPlot]

    allPlots = Prelude.concat $ Prelude.zipWith plotsForSbr [0..] sbrData

    layout = layout_title .~ printf "Effective ISO Speed vs N-Value - %s (%s)" filmName developer
      $ layout_plots .~ allPlots
      $ layout_legend .~ Just (def & legend_orientation .~ LOCols 1)
      $ layout_x_axis . laxis_title .~ "N-Value"
      $ layout_y_axis . laxis_title .~ "Effective ISO Speed"
      $ def

    color i =
      let numColors = Prelude.max 3 (Prelude.min 9 (length sbrData))
          palette = brewerSet Set1 numColors
      in opaque $ palette !! (i `mod` length palette)

