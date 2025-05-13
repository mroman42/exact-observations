module Main where

import Plotting    

main :: IO ()
main = do
  plotHeatMap [
    [1, 2, 3, 4, 4],
    [4, 2, 3, 3, 4],
    [9, 1, 0, 10, 2]
    ]