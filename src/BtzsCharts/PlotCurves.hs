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
import BtzsCharts.HDCurveFitting (HDCurve(..))
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Backend.Diagrams (renderableToFile)
import Data.Vector.Storable(toList)
import BtzsCharts.Types (Density)

plotHDCurves :: [HDCurve] -> Layout Density Density
plotHDCurves curves = layout
  where
    hdLine c =
      plot_lines_title .~ hdCurveLabel c
      $ plot_lines_values .~ [zip (toList $ relativeLogExposure c) (toList $ outputDensity c)]
      $ def
    layout = layout_title .~ "HD-Curve"
      $ layout_plots .~ Prelude.map (toPlot . hdLine) curves
      $ def


saveToFile :: ToRenderable p => p -> FilePath -> IO()
saveToFile layout path = do
  _ <- renderableToFile def path renderable
  return ()
  where
    renderable = toRenderable layout
