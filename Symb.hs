module Symb where

type Variable = String
data Symb = Real Double | Plus Symb Symb | Mult Symb Symb | Abs Symb | Signum Symb | Var Variable 
  | Integral Variable Symb | IntegralBounds Variable Symb Symb Symb | Delta Variable



instance Show Symb where
  show :: Symb -> String
  show (Real x) = show x
  show (Plus x y) = "(" ++ show x ++ " + " ++ show y ++ ")"
  show (Mult x y) = "(" ++ show x ++ " * " ++ show y ++ ")"
  show (Abs x) = "| " ++ show x ++ " |"
  show (Signum x) = "sig(" ++ show x ++ ")"
  show (Var s) = s
  show (Integral x s) = "int{" ++ x ++ "} " ++ show s
  show (IntegralBounds x a b s) = "int{" ++ x ++ " from " ++ show a ++ " to " ++ show b ++ "}" ++ show s
  show (Delta v) = "δ{" ++ v ++ "}"

fromDouble :: Double -> Symb
fromDouble = Real

instance Num Symb where
  (+) :: Symb -> Symb -> Symb
  (+) = Plus

  (*) :: Symb -> Symb -> Symb
  (*) = Mult 

  abs :: Symb -> Symb
  abs = Abs

  signum :: Symb -> Symb
  signum = Signum

  fromInteger :: Integer -> Symb
  fromInteger n = Real (fromInteger n)

  negate :: Symb -> Symb
  negate = Mult (Real (-1))


-- conditional :: Symb -> Symb
-- conditional 