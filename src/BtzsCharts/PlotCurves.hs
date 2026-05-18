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
    saveToFile
  ) where

import Control.Lens
import Data.Default.Class
import Data.Colour(opaque)
import Data.Colour.Palette.BrewerSet
import BtzsCharts.HDCurveFitting (HDCurve(..))
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
