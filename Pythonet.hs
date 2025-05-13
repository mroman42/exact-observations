{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}

module Pythonet where

import Data.List
import Data.Text qualified as TText
import Data.String.Interpolate
import Data.String.Here
import qualified Data.Set.Monad as Set
import System.Process
import Text.RawString.QQ

import Program

class Pythonize a where
  pythonize :: a -> String

instance Pythonize String where
  pythonize :: String -> String
  pythonize = id

instance Pythonize Expr where
  pythonize :: Expr -> String
  pythonize (Variable v) = v
  pythonize (Real r) = show r


executePython :: String -> IO String
executePython code = readProcess "python3" ["-c", code] ""

example2 :: Prog FunctionPy Expr
example2
  = Do ["m"] (Gen uniformDistribution) [Real 0, Real 1.0]
  $ Do ["y"] (Gen normalDistribution) [Variable "m", Real 1.0]
  $ Observe "y" 2.1
  $ Return [v "m"]

example3 :: Net FunctionPy Expr
example3
  = Node [Variable "x"] (Gen normalDistribution) [Real 0.0, Variable "t"]
  $ Open [v "x", v "t"] []


data PythonFunction = PythonFunction
  { functionName :: String
  , functionArgs :: [String]
  , functionDefn :: String
  }

instance Show PythonFunction where
  show :: PythonFunction -> String
  show (PythonFunction name args defn) = name

instance Pythonize PythonFunction where
  pythonize :: PythonFunction -> String
  pythonize (PythonFunction name args defn) = name

declare :: PythonFunction -> String
declare (PythonFunction name args defn) =
  "def " ++ name ++ "(" ++ intercalate ", " args ++ "):\n" ++
  indent defn
  where
    indent = unlines . map ("    " ++) . lines

exponential :: PythonFunction
exponential = PythonFunction
  { functionName = "exponential"
  , functionArgs = ["x"]
  , functionDefn = "return sp.exp(x)"
  }

normalDistribution :: PythonFunction
normalDistribution = PythonFunction
  { functionName = "normal_distribution"
  , functionArgs = ["x", "mu", "sigma"]
  , functionDefn = "return (1/(sigma * sp.sqrt(2 * sp.pi))) * sp.exp(-0.5 * ((x - mu)/sigma)**2)"
  }

uniformDistribution :: PythonFunction
uniformDistribution = PythonFunction
  { functionName = "uniform_distribution"
  , functionArgs = ["x", "a", "b"]
  , functionDefn = "return sp.Piecewise((0,x <= a) , (1/(b-a), x <= b), (0, True))" -- "return 1/(b - a) if a <= x <= b else 0" 
  }

poissonDistribution :: PythonFunction
poissonDistribution = PythonFunction
  { functionName = "poisson_distribution"
  , functionArgs = ["x", "lambda_"]
  , functionDefn = "return (sp.exp(-lambda_) * lambda_**x) / sp.factorial(x)"
  }


type FunctionPy = Func PythonFunction


pythonizeFunction :: Func PythonFunction -> [Expr] -> [Expr] -> String
pythonizeFunction (Gen f) [] xs = functionName f ++ "(" ++ intercalate ", " (map pythonize xs) ++ ")"
pythonizeFunction (Gen f) ys xs = functionName f ++ "(" ++ intercalate ", " (map pythonize ys) ++ ", " ++ intercalate ", " (map pythonize xs) ++ ")"

pythonizeNet :: Int -> Net (Func PythonFunction) Expr -> String
pythonizeNet n (Node ys f xs p) = "w" ++ show n ++ " = " ++ pythonizeFunction f ys xs ++ "\n" ++ pythonizeNet (n+1) p
pythonizeNet n (Open os cs) = 
    "oo = " ++ multiplication ++ "/ sp.integrate(" ++ multiplication ++ ", " ++ openIntegrations os ++ ")"
  where
    multiplication = intercalate " * " (("w" ++) . show <$> [0..n-1])
    openIntegrations os = intercalate ", " $ map (\x -> "(" ++ show x ++ ", -2, 2)") os

pythonHeader :: String
pythonHeader = [__i|
    import sympy as sp
    import numpy as np
    import matplotlib.pyplot as plt
    |] ++ "\n"

pythonFooter :: String
pythonFooter = [__i|
    f_numeric = sp.lambdify(m, oo, modules="numpy")
    f_numeric_vectorized = np.vectorize(f_numeric)
    m_values = np.linspace(-2, 2, 500)  
    f_values = f_numeric_vectorized(m_values)
    
    plt.figure(figsize=(8, 6))
    plt.plot(m_values, f_values, label='w0(m)')
    plt.title('Plot of out(m)')
    plt.xlabel('m')
    plt.ylabel('out(m)')
    plt.legend()
    plt.grid(True)
    plt.show()
    |]

pythonHeatmapFooter :: String
pythonHeatmapFooter = [__i|
    w0_numeric = sp.lambdify((x, t), w0, modules="numpy")

    x_values = np.linspace(-1, 1, 500)
    t_values = np.linspace(0.1, 1, 500)

    X, T = np.meshgrid(x_values, t_values)
    W0_values = w0_numeric(X, T)

    plt.figure(figsize=(10, 8))
    plt.contourf(T, X, W0_values, levels=50, cmap="viridis")
    plt.colorbar(label="probability")
    plt.title("Heat Map of w0(x, t)")
    plt.xlabel("t")
    plt.ylabel("x")
    plt.show()
    |]

pythonizeNetwork :: Net (Func PythonFunction) Expr -> String
pythonizeNetwork net = unlines
    [ pythonHeader
    , "# function declaration"
    , networkHeader net
    , "# variable declaration"
    , unlines $ (\v -> v ++ " = sp.Symbol(\'" ++ v ++ "\')" ) <$> nub (variableList net)
    , "# probabilistic computations"
    , pythonizeNet 0 net
    , "\n" ++ "# plotting"
    , pythonFooter
    ]

pythonizeHeatmap :: Net (Func PythonFunction) Expr -> String
pythonizeHeatmap net = unlines
    [ pythonHeader
    , "# function declaration"
    , networkHeader net
    , "# variable declaration"
    , unlines $ (\v -> v ++ " = sp.Symbol(\'" ++ v ++ "\')" ) <$> nub (variableList net)
    , "# probabilistic computations"
    , pythonizeNet 0 net
    , "\n" ++ "# plotting"
    , pythonHeatmapFooter
    ]
    

networkHeader :: Net (Func PythonFunction) Expr -> String
networkHeader net = unlines $ declare <$> getFunctionsNet net
