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
import LazyPPL (uniform)
import LazyPPL.Distributions (poisson)


executePython :: String -> IO String
executePython code = readProcess "python3" ["-c", code] ""

example2 :: Prog FunctionPy Expr
example2 
  = Do ["x"] (Gen uniformDistribution) []
  $ Return [v "x"]

type Var = String

data PythonFunction = PythonFunction 
  { functionName :: String
  , functionArgs :: [String]
  , functionDefn :: String
  }

instance Show PythonFunction where
  show :: PythonFunction -> String
  show (PythonFunction name args defn) = name
  
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
  , functionDefn = "return 1/(b - a) if a <= x <= b else 0" 
  }

poissonDistribution :: PythonFunction
poissonDistribution = PythonFunction
  { functionName = "poisson_distribution"
  , functionArgs = ["x", "lambda_"]
  , functionDefn = "return (sp.exp(-lambda_) * lambda_**x) / sp.factorial(x)" 
  }

data Func f = Gen f | Conditional Int (Func f) deriving Eq

instance Show s => Show (Func s) where
  show :: Func s -> String
  show (Gen s) = show s
  show (Conditional i f) = show f ++ "{" ++ show i ++ "}"

type Function = Func String
type FunctionPy = Func PythonFunction

data Expr = Variable Var | Real Double deriving (Eq, Ord)

data Prog f e
  = Do [Var] f [e] (Prog f e)
  | Observe Var Double (Prog f e)
  | Return [Expr]

type Program = Prog Function Expr


v :: Var -> Expr
v = Variable

instance Show Expr where
  show :: Expr -> String
  show (Variable v) = v
  show (Real r) = show r

instance (Show f) => Show (Prog f Expr) where
  show :: (Show f) => Prog f Expr -> String
  show (Do ys f xs program) = show ys ++ " <- " ++ show f ++ show xs ++ "\n" ++ show program
  show (Observe v d program) = "observe " ++ show v ++ " == " ++ show d ++ "\n" ++ show program
  show (Return vars) = "return" ++ show vars

reduce :: Prog (Func f) Expr -> Prog (Func f) Expr
reduce (Observe v d program) = Observe v d $ reduce program
reduce (Do ys f xs (Observe v d program)) =
   case v `elemIndex` ys of
    Nothing -> reduce $ Observe v d (Do ys f xs program) -- Interchange
    Just i  -> reduce $ Do (delete v ys) (Conditional i f) (Real d : xs) (replaceVarValue v d program)
reduce (Return xs) = Return xs
reduce other = other

replace :: (Expr -> Expr) -> Prog f Expr -> Prog f Expr
replace h (Return xs) = Return (h <$> xs)
replace h (Do ys f xs p) = Do ys f (h <$> xs) (replace h p)
replace h (Observe v d p) = Observe v d (replace h p)

replaceVarValue :: Var -> Double -> Prog f Expr -> Prog f Expr
replaceVarValue v d = replace (\case
    Variable w | w == v -> Real d
    other               -> other)

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

formatProgram :: (Pythonize f) => Prog (Func f) Expr -> String
formatProgram program = header ++ "\n" ++ pythonize 0 program ++ footer
  where
    header = [__i|
      import sympy as sp
      def f(u1,u2,u3,u4): return sp.sin(u1) + sp.cos(u3)
      def g(u1,u2,u3): return sp.sin(u1) + sp.cos(u3)
      x = sp.Symbol('x')
      y = sp.Symbol('y')
      z = sp.Symbol('z')
      v1 = sp.Symbol('v1')|]
    footer = ""

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
    pythonize f ++ "(" ++ intercalate "," ys ++ "," ++ intercalate "," (map show xs) ++ ")"
pythonizeFunction (Conditional i f) ys xs = 
    "(" ++ pythonizeFunction f ys xs ++ ")/(" ++ " sp.integrate(" ++ pythonizeFunction f ys (setAt i (Variable v) xs) ++ ", " ++ v ++ "))"
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

