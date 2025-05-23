{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}

module Plotting (plotHeatMap) where

import Data.List
import Data.Text qualified as TText
import Data.String.Interpolate
import Data.String.Here
import qualified Data.Set.Monad as Set
import System.Process
import Text.RawString.QQ
import Control.Monad

fromPython :: String -> IO String
fromPython code = readProcess "python3" ["-c", code] ""

executePython :: String -> IO ()
executePython code = do
  _ <- readProcess "python3" ["-c", code] ""
  return ()

writeCSV :: FilePath -> [[Double]] -> IO ()
writeCSV filePath dataList = do
  let csvContent = unlines $ map (intercalate "," . map show) dataList
  writeFile filePath csvContent  

example :: [[Double]]
example = 
    [[1, 2, 3, 4, 4],
     [4, 2, 3, 3, 4],
     [9, 1, 0, 10, 2]]  
     
     
plotHeatMap :: [[Double]] -> IO ()
plotHeatMap map = do
  writeCSV "heatmap.csv" map
  executePython [__i|
    import numpy as np
    import seaborn as sns
    import matplotlib.pyplot as plt
    from matplotlib.colors import LogNorm
    sns.set_theme(style="white")
    cmap = sns.color_palette("mako", as_cmap=True)
    heatmap_data = np.loadtxt("heatmap.csv", delimiter=",")
    sns.heatmap(heatmap_data, cmap=cmap)
    plt.show()|]