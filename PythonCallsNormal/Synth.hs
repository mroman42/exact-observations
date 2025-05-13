{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}

module Synth where

import Data.List
import Data.Text qualified as TText
import Data.String.Interpolate
import Data.String.Here
import qualified Data.Set.Monad as Set
import System.Process
import Text.RawString.QQ

import Program


executePython :: String -> IO String
executePython code = readProcess "python3" ["-c", code] ""

example2 :: Prog FunctionPy Expr
example2 
  = Do ["m"] (Gen uniformDistribution) [Real 0.5, Real 1.0]
  $ Do ["y"] (Gen normalDistribution) [Variable "m", Real 1.0]
  $ Observe "y" 2.1
  $ Return [v "m"]



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


example1 :: Program
example1
  = Do ["y","y1","y2"] (Gen "f") [v "x"]
  $ Observe "y1" 4.3
  $ Observe "y2" 2.1
  $ Do ["z"] (Gen "g") [v "y", v "y1"]
  $ Return [v "y"]



exprGetVars :: Expr -> Set.Set Var
exprGetVars (Variable a) = Set.singleton a
exprGetVars (Real _) = Set.empty

exprsGetVars :: Set.Set Expr -> Set.Set Var
exprsGetVars = foldMap exprGetVars

hiddenVars :: Set.Set Var -> Prog f Expr -> Set.Set Var
hiddenVars hidden (Return vars)   = hidden `Set.difference` foldMap exprGetVars vars
hiddenVars hidden (Observe _ _ p) = hiddenVars hidden p
hiddenVars hidden (Do ys _ _ p)   = hiddenVars (hidden `Set.union` Set.fromList ys) p

getHiddenVars :: Prog f Expr -> [Var]
getHiddenVars = Set.toList . hiddenVars Set.empty

formatProgramHeader :: Prog (Func PythonFunction) Expr -> String
formatProgramHeader program = unlines $ declare <$> getFunctionLabels program

formatProgram :: Prog (Func PythonFunction) Expr -> String
formatProgram program = header ++ "\n" ++ pythonize 0 program ++ "\n" ++ footer ++ "\n"
  where
    header = [__i|
      import sympy as sp
      import numpy as np
      import matplotlib.pyplot as plt

      from sympy.abc import x, y, z, m
      def f(u1,u2,u3,u4): return sp.sin(u1) + sp.cos(u3)
      def g(u1,u2,u3): return sp.sin(u1) + sp.cos(u3)
      v0 = sp.Symbol('v0')
      v1 = sp.Symbol('v1')
      |]
      ++ "\n" ++ formatProgramHeader program
    footer = [__i|
      f_numeric = sp.lambdify(m, w1, modules="numpy")
      f_numeric_vectorized = np.vectorize(f_numeric)
      m_values = np.linspace(-10, 10, 500)  
      f_values = f_numeric_vectorized(m_values)
      
      plt.figure(figsize=(8, 6))
      plt.plot(m_values, f_values, label='w0(m)')
      plt.title('Plot of w0(m)')
      plt.xlabel('m')
      plt.ylabel('w0(m)')
      plt.legend()
      plt.grid(True)
      plt.show()
      |]

      -- f_numeric = sp.lambdify(m, w1, 'numpy')

      -- values = np.linspace(-10, 10, 500)
      -- f_values = f_numeric(values)

      -- plt.figure(figsize=(8, 6))
      -- plt.plot(values, f_values, label='f(v)')
      -- plt.title('Plot of f(v)')
      -- plt.xlabel('v')
      -- plt.ylabel('f(v)')
      -- plt.legend()
      -- plt.grid(True)
      -- plt.show()

    pythonize n (Do ys f xs program) =
      "w" ++ show n ++ " = " ++ pythonizeFunction f ys xs ++ "\n" ++ pythonize (n+1) program
    pythonize n (Observe v d program) =
      "an observe statement"
    pythonize n (Return vars) = 
      "print( " ++ 
      intercalate ", " (map (\n -> "w" ++ show n) [0..n-1])
      ++ " )" 

pythonizeFunction :: (Pythonize f) => Func f -> [Var] -> [Expr] -> String
pythonizeFunction (Gen f) ys xs =
  if null ys
  then pythonize f ++ "(" ++ intercalate "," (map show xs) ++ ")"
  else pythonize f ++ "(" ++ intercalate "," ys ++ "," ++ intercalate "," (map show xs) ++ ")"
pythonizeFunction (Conditional i f) ys xs = 
    "(" ++ pythonizeFunction f ys xs ++ ")/(" ++ " sp.integrate(" ++ pythonizeFunction f ys (setAt i (Variable v) xs) ++ ", (" ++ v ++ ", -10, 10)" ++ "))"
  where
    v = "v" ++ show i

class Pythonize a where
  pythonize :: a -> String

instance Pythonize String where
  pythonize :: String -> String
  pythonize = id

setAt :: Int -> a -> [a] -> [a]
setAt i x xs = take i xs ++ [x] ++ drop (i + 1) xs

main :: IO ()
main = do
  output <- executePython [__i|
    import sympy as sp
    x = sp.Symbol('x')
    
    expression = sp.sin(x)**2 - sp.cos(x)**2
    simplified_expression = sp.simplify(expression)
    integral = sp.integrate(simplified_expression, x)

    print(f"Simplified Expression: {simplified_expression}")
    print(f"Integral: {integral}")
    |]
  putStrLn output


pythonizeNet :: Int -> Net (Func PythonFunction) Expr -> String
pythonizeNet n (Node ys f xs p) = "w" ++ show n ++ " = " ++ pythonizeFunction f ys xs ++ "\n" ++ pythonizeNet (n+1) p
pythonizeNet n (Open xs) = "\n"

pythonizeNetwork :: Net (Func PythonFunction) Expr -> String
pythonizeNetwork net = header ++ "\n" ++ pythonizeNet 0 net ++ "\n" ++ footer ++ "\n"
  where
    header = [__i|
      import sympy as sp
      import numpy as np
      import matplotlib.pyplot as plt

      from sympy.abc import x, y, z, m
      def f(u1,u2,u3,u4): return sp.sin(u1) + sp.cos(u3)
      def g(u1,u2,u3): return sp.sin(u1) + sp.cos(u3)
      v0 = sp.Symbol('v0')
      v1 = sp.Symbol('v1')
      |]
      ++ "\n" ++ formatProgramHeader p
    footer = [__i|
      f_numeric = sp.lambdify(m, w1, modules="numpy")
      f_numeric_vectorized = np.vectorize(f_numeric)
      m_values = np.linspace(-10, 10, 500)  
      f_values = f_numeric_vectorized(m_values)
      
      plt.figure(figsize=(8, 6))
      plt.plot(m_values, f_values, label='w0(m)')
      plt.title('Plot of w0(m)')
      plt.xlabel('m')
      plt.ylabel('w0(m)')
      plt.legend()
      plt.grid(True)
      plt.show()
      |]