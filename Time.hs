module Time where

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Diagrams
import Data.Colour
import Data.Colour.Names
import Data.Default.Class (def)
import Control.Monad (forM_)

-- Function to calculate the Normal(0, t) density
normalDensity :: Double -> Double -> Double
normalDensity t x = (1 / sqrt (2 * pi * t)) * exp (- ((x ^ 2) / (2 * t)))

-- Generate heatmap data
generateHeatmapData :: (Double -> Double -> Double) -> [Double] -> [Double] -> [(Double, Double, Double)]
generateHeatmapData function times values =
  [ (t, x, function t x) | t <- times, x <- values ]

-- Plot heatmap
plotHeatmap :: IO ()
plotHeatmap = toFile def "heatmap.svg" $ do
  layout_title .= "Heatmap of Normal(0, t)"
  let times = [0.1, 0.2 .. 5.0]  -- Time values (horizontal axis)
      values = [-3, -2.9 .. 3]   -- Normal distribution values (vertical axis)
      heatmapData = generateHeatmapData times values
  plot $ heatMap
    [ (t, x, z) | (t, x, z) <- heatmapData, z > 0 ]  -- Filter out invalid values
    (0.1, 5.0) (-3, 3)  -- Axis ranges
    (withOpacity blue 0.5, withOpacity red 0.5)  -- Color gradient

