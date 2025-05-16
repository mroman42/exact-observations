module Main where

import Data.List

import Plotting
import NormalExample
import Observation


dynamicsGrid :: Dynamics Double Double -> [[Double]]
dynamicsGrid f = scaleGrid grid
  where
    increment = 1 / 40
    grid = [[f t 0 x | t <- [0,increment..10]] | x <- [-5,-5+increment..5]]
    scale = maximum $ map minimum grid
    scaleGrid = map (map (min (scale * 5)))

dynamicsGrid2 :: Dynamics Double Double -> [[Double]]
dynamicsGrid2 f = scaleGrid grid
  where
    increment_t = 1 / 8
    increment_x = 1 / 4
    grid = [[f t 0 x | t <- [0,increment_t..20]] | x <- [0,increment_x..20]]
    scale = maximum $ map maximum grid
    scaleGrid = map (map (min (scale * 0.9)))

normalizeColumn :: [Double] -> [Double]
normalizeColumn d =
    let s = sum (filter (not . isNaN) d) in
      if s == 0
        then map (const 0) d
        else map (/ s) d

filterNan :: Double -> Double
filterNan d = if isNaN d then 1000 else 0

normalizeGrid :: [[Double]] -> [[Double]]
normalizeGrid = transpose . map normalizeColumn . transpose

normalizeWithMark :: [[Double]] -> [[Double]]
normalizeWithMark = transpose . map (\c -> sum c : normalizeColumn c) . transpose

main :: IO ()
main = 
  -- plotHeatMap $ dynamicsGrid $ Observation.normal `withObservations` [(3,0.6), (5,0.1), (6,-0.5)]
  plotHeatMap $ dynamicsGrid2 $ Observation.poissonDouble 1 `withObservations` [(3,5), (10,7)]

