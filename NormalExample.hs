module NormalExample where

type Time = Double

-- Normal example
normal :: Double -> Double -> Double -> Double
normal x mu sigma2 = if abs sigma2 < 0.001 && abs (x - mu) > 0.1 then 0 else 
  (1 / sqrt (2 * pi * sigma2)) * exp (- ((x - mu) ^ 2 / (2 * sigma2)))

cleannormal :: Double -> Double -> Double -> Double
cleannormal x mu sigma2 =
  (1 / sqrt (2 * pi * sigma2)) * exp (- ((x - mu) ^ 2 / (2 * sigma2)))


normalGrid :: [[Double]]
normalGrid = [[min (normal x 0 t) 0.3 | t <- [0,0.025..10]] | x <- [-5,-4.95..5]]

normalWithObservations :: (Double, Double) -> [(Double,Double)] -> Double -> Double -> Double
normalWithObservations (t0,o0) [] x t = normal x o0 (t - t0)
normalWithObservations (t0,o0) ((t1,o1):obs) x t = 
    if t < t1 
        then normalBridge (t0, o0) (t1, o1) x t
        else normalWithObservations (t1,o1) obs x t


normalBridge :: (Time, Double) -> (Time, Double) -> Double -> Time -> Double
normalBridge (t0,o0) (t1,o1) x t = normal x (o0 + (o1 - o0) * (t - t0) / (t1 - t0)) (min (t - t0) (t1 - t))

normalExample :: Double -> Double -> Double
normalExample = normalWithObservations (0,0) [(1,1), (4,-2), (8,3)]

normalGrid2 :: [[Double]]
normalGrid2 = [[min (normalExample x t) 0.6 | t <- [0,0.020..10]] | x <- [-5,-4.98..5]]
      
