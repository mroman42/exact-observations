{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}

module Program where

import Data.List
import Data.Text qualified as TText
import Data.String.Interpolate
import Data.String.Here
import qualified Data.Set.Monad as Set
import System.Process
import Text.RawString.QQ
import Data.Map qualified as Map

type Var = String

data Expr = Variable Var | Real Double deriving (Eq, Ord)

instance Show Expr where
  show :: Expr -> String
  show (Variable v) = v
  show (Real r) = show r


v :: Var -> Expr
v = Variable


data Func f = Gen f | Conditional Int (Func f) deriving Eq

instance Show s => Show (Func s) where
  show :: Func s -> String
  show (Gen s) = show s
  show (Conditional i f) = show f ++ "{" ++ show i ++ "}"

type Function = Func String


data Prog f e
  = Do [Var] f [e] (Prog f e)
  | Observe Var Double (Prog f e)
  | Return [e]

type Program = Prog Function Expr

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
reduce (Do ys f xs program) = Do ys f xs $ reduce program
reduce (Return xs) = Return xs
-- reduce other = other

replace :: (Expr -> Expr) -> Prog f Expr -> Prog f Expr
replace h (Return xs) = Return (h <$> xs)
replace h (Do ys f xs p) = Do ys f (h <$> xs) (replace h p)
replace h (Observe v d p) = Observe v d (replace h p)

replaceVarValue :: Var -> Double -> Prog f Expr -> Prog f Expr
replaceVarValue v d = replace (\case
    Variable w | w == v -> Real d
    other               -> other)

getFunctionLabels :: Prog (Func f) Expr -> [f]
getFunctionLabels (Return xs) = []
getFunctionLabels (Observe v d p) = getFunctionLabels p
getFunctionLabels (Do ys (Gen f) xs p) = f : getFunctionLabels p
getFunctionLabels (Do ys (Conditional i f) xs p) = getFunctionLabels $ Do ys f xs p



data Net f e
  = Node [e] f [e] (Net f e)
  | Open [e] [e]
type Network = Net Function Expr

instance (Show f) => Show (Net f Expr) where
  show :: (Show f) => Net f Expr -> String
  show (Node ys f xs p) = show ys ++ " <- " ++ show f ++ show xs ++ "\n" ++ show p
  show (Open os cs) = "open " ++ show os ++ " closed " ++ show cs

getSubstitutions :: Prog f Expr -> Map.Map Var Double
getSubstitutions (Return xs) = Map.empty
getSubstitutions (Observe v d p) = Map.insert v d $ getSubstitutions p
getSubstitutions (Do ys f xs p) = getSubstitutions p

substitute :: Map.Map Var Double -> Expr -> Expr
substitute m (Variable v) = case Map.lookup v m of
  Just d  -> Real d
  Nothing -> Variable v
substitute m (Real r) = Real r

substituteVar :: Map.Map Var Double -> Var -> Expr
substituteVar m v = case Map.lookup v m of
  Just d  -> Real d
  Nothing -> Variable v

substituteToNetwork :: Map.Map Var Double -> Prog f Expr -> Net f Expr
substituteToNetwork m (Return xs) = Open (substitute m <$> xs) []
substituteToNetwork m (Observe v d p) = substituteToNetwork m p
substituteToNetwork m (Do ys f xs p) = Node (substituteVar m <$> ys) f (substitute m <$> xs) (substituteToNetwork m p)

toNetwork :: Prog f Expr -> Net f Expr
toNetwork p = substituteToNetwork (getSubstitutions p) p

varsOnly :: [Expr] -> [Var]
varsOnly [] = []
varsOnly (Variable v : xs) = v : varsOnly xs
varsOnly (Real _ : xs) = varsOnly xs

variableList :: Net f Expr -> [Var]
variableList (Node ys f xs p) = varsOnly ys ++ varsOnly xs ++ variableList p
variableList (Open os cs) = varsOnly os ++ varsOnly cs

getFunctionsNet :: Net (Func f) Expr -> [f]
getFunctionsNet (Node ys (Gen f) xs p) = f : getFunctionsNet p
getFunctionsNet (Open os cs) = []