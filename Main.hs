module Main where

import Plotting

normal :: Double -> Double -> Double -> Double
normal x mu sigma2 = (1 / sqrt (2 * pi * sigma2)) * exp (- ((x - mu) ^ 2 / (2 * sigma2)))

normalGrid :: [[Double]]
normalGrid = [[min (normal x 0 t) 0.3 | t <- [0,0.2..10]] | x <- [-5,-4.8..5]]

main :: IO ()
main = do
  plotHeatMap normalGrid