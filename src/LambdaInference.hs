module LambdaInference where

{-
 - TODO: 
 - 	Smart Environment: looks up integers (1,2,3,...) booleans (True, False), strings, chars
 - 	posibility to insert free variables in the environment: +, -, neg, if-then-else
 -}

import LambdaData
import LambdaParser
import Control.Monad.State
import qualified Data.Map as M
import Data.Map (Map(..))  
import Maybe


type Environment = Map (Expr) Type

-- ------------------------------------ --
-- Creates an empty environment		--
-- ------------------------------------ --
emptyEnvironment :: Environment
emptyEnvironment = M.empty

-- ------------------------------------ --
-- Inserts into an environment		--
-- ------------------------------------ --
insertIntoEnv :: Expr -> Type -> Environment -> Environment
insertIntoEnv = M.insert



-- ------------------------------------ --
-- Infers the type of an expresion 	--
-- according to the given environment 	--
-- ------------------------------------ --
inferTypeRec :: Expr -> Environment -> (Maybe Type)
inferTypeRec ( Var x)  		env 	= M.lookup (Var x) env

inferTypeRec ( Appl x y) 	env	= let 	tx = inferTypeRec x env
						ty = inferTypeRec y env 
					  in
						if tx == Nothing || ty == Nothing
							then 	Nothing
							else 	case fromJust tx of
								(Function d c) 	-> if d == fromJust ty then Just c else Nothing
								_		-> Nothing

inferTypeRec ( Lambda v t exp)	env 	= let 	newEnv = M.insert (Var v) t env 
						tBody  = inferTypeRec exp newEnv 
					  in 
						if tBody == Nothing then Nothing else Just (Function t (fromJust tBody))


-- -------------------------------------------- --
-- Wrapper for the inferTypeRec function	--
-- invokes it with an empty environment		--
-- -------------------------------------------- --
inferType :: Expr -> Maybe Type
inferType x = inferTypeRec x emptyEnvironment



-- --------------------------------------------------------------------- --
--  Based on the statements in the let "header" this function constructs -- 
-- an environment for typing expresions in the let "body"		 --
-- --------------------------------------------------------------------- --	
inferLetEnv :: [(String, Expr)] -> Environment -> Environment
inferLetEnv defList env	= let 	defs = M.fromList defList 
				inferStatement env stmt = case inferTypeRec (snd stmt) env of
								Nothing -> env
								Just t  -> insertIntoEnv (Var $ fst stmt) t env
				in
			   	foldl inferStatement env defList 


-- --------------------------------------------	--
-- Infers the type of expresions in the body 	--
-- of a top-level let expresion			--
-- -------------------------------------------- --
inferLetExp :: Environment -> ParsedLet -> [Maybe Type]
inferLetExp env ( m , exps)  = let 	newEnv = inferLetEnv m env
					in map (flip inferTypeRec newEnv) exps


-- --------------------------------------------	--
-- Checks the type of an expression represented --
-- by a string					--
-- -------------------------------------------- --
checkStringType :: String -> Maybe Type
checkStringType = inferType . fst . head . parse lambdaExp 




main2 pth = do	
	txt <- readFile pth
	return ( inferLetExp emptyEnvironment (fst (head  (parse letExpr txt)))) 



main3 pth = do
	txt <- readFile pth
	return ( fst (head  (parse lambdaFile txt)))
