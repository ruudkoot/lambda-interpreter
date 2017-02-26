{-# LANGUAGE TypeSynonymInstances #-}

module LambdaData where

import Data.Either
import qualified Data.Map as M
import Data.Map (Map(..))  

type Var          = String
type Idx          = Either Int Var
type TypeVar      = Var

type Env = [(Var,Int)]

data Type         = Type     TypeVar
                  | Function Type Type
                  deriving (Eq, Ord)
                  
type TypedVariable = (Var , Type)

data LambdaExpr a = Var        a
                  | Appl       (LambdaExpr a) (LambdaExpr a)
                  | Lambda     Var Type   (LambdaExpr a)
                  | TypeAppl   (LambdaExpr a) (LambdaExpr a)
                  | TypeLambda TypeVar        (LambdaExpr a)
                  deriving (Eq, Ord)

type NamedExpr    = LambdaExpr Var
type DeBruijnExpr = LambdaExpr Idx

type Expr         = NamedExpr           -- backwards compatibility

--type Ndex = Left
--type Name = Right

type ParsedFile   = ( (Map String Expr) , [ Expr ] )
type ParsedLet    = ( [ (String, Expr)] , [Expr])

instance Show Expr where
	show (Var v) = v
	show (Appl e1 e2) = "(" ++ show e1 ++ " " ++ show e2 ++")"
	show (Lambda v t e) = "(\\(" ++ v ++ ":" ++ show t ++ ")->" ++ show e ++ ")"

instance Show Type where
	show ( Type t) = t
	show ( Function t1 t2) = "(" ++ show t1 ++ "->" ++ show t2 ++ ")"

