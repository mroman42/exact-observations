module Observation where

import Data.List (sortOn)

type Dynamics r x = r -> x -> (x -> r)

normal :: (Floating r, Ord r) => Dynamics r r
normal sigma2 mu x = if abs sigma2 < 0.02 && abs (x - mu) > 0.1 then 0 else 
  (1 / sqrt (2 * pi * sigma2)) * exp (- ((x - mu) ^ 2 / (2 * sigma2)))

poisson :: (Floating r) => r -> Dynamics r Integer
poisson lambda t x0 x = 
  if x < x0 
    then 0
    else (lambda * t) ^ (x - x0) / fromInteger (product [1,2..(x - x0)]) * exp (- (lambda * t))

poissonDouble :: Double -> Dynamics Double Double
poissonDouble lambda t x0 x = 
  if x0 == fromIntegral (floor x0) && x == fromIntegral (floor x)
    then poisson lambda t (floor x0) (floor x) 
    else 0

cleannormal :: (Floating r, Ord r) => Dynamics r r
cleannormal sigma2 mu x =
  (1 / sqrt (2 * pi * sigma2)) * exp (- ((x - mu) ^ 2 / (2 * sigma2)))

withObservation :: (Num t, Ord t, Num s, Ord s) => Dynamics t s -> (t, s) -> Dynamics t s
withObservation f (ot,ox) t m x =
  if t < ot 
    then f t  m x  * f (ot - t) x ox
    else f ot m ox * f (t - ot) ox x
        

intervalling :: (Num t, Ord t, Num r) => r -> [(t, r)] -> [(t, r, r)]
intervalling m xs = zipWith toInterval ((0, m) : sorted) sorted
  where
    sorted = sortOn fst xs
    toInterval (t1, x1) (t2, x2) = (t2 - t1, x1, x2)

withObservations :: (Num r, Ord r, Num s, Ord s) => Dynamics r s -> [(r, s)] -> Dynamics r s
withObservations f obs t m x =
  product $ map (\(t,m,x) -> f t m x) $ intervalling 0 ((t,x) : obs)