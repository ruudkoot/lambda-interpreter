{-# LANGUAGE TypeSynonymInstances #-}

module LambdaInterpreter where

import Data.List
import Data.Maybe
import qualified Data.Map as Map 

import Debug.Trace

import LambdaData

-- lambda expression catamorphism stuff

type LambdaAlgebra a r =
        ( a   -> r              -- Var
        , r   -> r -> r         -- Appl
        , Var -> Type -> r -> r         -- Lambda
        )
        
type NamedAlgebra    r = LambdaAlgebra Var r
type DeBruijnAlgebra r = LambdaAlgebra Idx r

foldExpr :: LambdaAlgebra a r -> LambdaExpr a -> r
foldExpr q@(v, a, l) (Var    n  ) = v n
foldExpr q@(v, a, l) (Appl   e f) = a (foldExpr q e) (foldExpr q f)
foldExpr q@(v, a, l) (Lambda n t e) = l n t            (foldExpr q e)


-- evalutation

subst m = toNamedExpression . (\x-> substDef x m) . toDeBruijnExpression

--eval = toNamedExpression . betaReduce . toDeBruijnExpression

eval :: (Map.Map String Expr) -> Expr -> Expr
eval m = toNamedExpression . betaReduce . (\x-> substDef x m) . toDeBruijnExpression

--eval pf = map (toNamedExpression . betaReduce) (toDebruijn pf)

toDebruijn :: ParsedFile -> [DeBruijnExpr]
toDebruijn  (m , es) = map (\x->substDef x m) (map toDeBruijnExpression es)

substDef :: DeBruijnExpr -> (Map.Map String Expr) -> DeBruijnExpr
substDef e m = foldExpr varSubstAlgebra e (substDefInDef m)

substDefInDef :: (Map.Map String Expr) -> (Map.Map String DeBruijnExpr)
substDefInDef m = Map.map (\x -> substDef x m) dmb
		  where dmb = (Map.map toDeBruijnExpression m)

varSubstAlgebra :: DeBruijnAlgebra ((Map.Map String DeBruijnExpr) -> DeBruijnExpr)
varSubstAlgebra = ( \n m -> fromMaybe (Var n) (Map.lookup (either (\_->"") id n) m),
		    \e f m -> Appl (e m) (f m),
		    \n t e m -> Lambda n t (e m)
    		   )	

-- fixedpoint

--fixedpoint :: (Eq a, Show a) => (a -> a) -> a -> a
fixedpoint f x = fixedpoint' f x (f x)

fixedpoint' f x x' | x == x'   = x
                   | otherwise = --trace ( "# " ++ show (toNamedExpression x)) $
                                        fixedpoint' f x' (f x')


-- pretty crappy printer

showAlgebra :: (Show a) => LambdaAlgebra a String
showAlgebra = ( show
              , \a b -> "("   ++ a ++ " " ++ b ++ ")"
              , \a t b -> "(\\" ++ a ++ ":" ++ show t ++ "." ++ b ++ ")"
              )

--instance (Show a) => Show (LambdaExpr a) where
--        show = foldExpr showAlgebra
        
instance Show DeBruijnExpr where
        show = foldExpr showAlgebra


-- Find free varialbes in an expression

freeAlgebra :: NamedAlgebra [Var]
freeAlgebra = ( \x -> [x]
              , union
              , \a _ b -> b \\ [a]
              )

free = foldExpr freeAlgebra

freeAlgebra' :: DeBruijnAlgebra [Var]
freeAlgebra' = ( \x -> case x of
                                (Left  y) -> []
                                (Right y) -> [y]
              , union
              , \a _ b -> b -- \\ [a]
              )

free' = foldExpr freeAlgebra'


-- beta reduction

betaReduce :: DeBruijnExpr -> DeBruijnExpr
--betaReduce = fixedpoint (\e->foldExpr betaReduction e)
betaReduce = fixedpoint betaReduction

-- single betareduction in applicative order (call-by-value, strict evaluation)
betaReduction :: DeBruijnExpr -> DeBruijnExpr
betaReduction e = let (eb, ee) = betaReduction' e
                   in ee

betaReduction' :: DeBruijnExpr -> (Bool, DeBruijnExpr)
betaReduction' (Var n)                 = (False, Var n)
betaReduction' (Appl (Lambda n _ e) f) = (True, substitute e f)
betaReduction' (Appl e              f) = let (eb, ee) = betaReduction' e
                                             (fb, fe) = betaReduction' f
                                          in (eb||fb, Appl ee (if eb then f else fe))
betaReduction' (Lambda n t e)          = let (eb, ee) = betaReduction' e
                                          in (eb, Lambda n t ee)

{-
betaReduction :: DeBruijnAlgebra DeBruijnExpr
betaReduction = ( \n   -> Var n
                , \e f -> betaReduceAppl e f
                , \n t e -> Lambda n t e
                )

betaReduceAppl :: DeBruijnExpr -> DeBruijnExpr -> DeBruijnExpr
betaReduceAppl (Lambda n _ e) f = substitute e f
betaReduceAppl e              f = Appl e f
-}

substitute :: DeBruijnExpr -> DeBruijnExpr -> DeBruijnExpr
substitute e f = foldExpr substitutionAlgebra e f 0

substitutionAlgebra :: DeBruijnAlgebra (DeBruijnExpr -> Int -> DeBruijnExpr)
substitutionAlgebra = ( \n     f i -> either (\x -> if (x==i)
                                                    then (rebind f i)
                                                    else (if (x>i) then (Var (Left (x-1))) else (Var n))
                                             )
                                             (\x -> Var n)
                                             n
                      , \g h   f i -> Appl (g f i) (h f i)
                      , \n t h f i -> Lambda n t (h f (i+1))
                      )
                      
rebind :: DeBruijnExpr -> Int -> DeBruijnExpr
rebind e i = foldExpr rebindAlgebra e i 0

rebindAlgebra :: DeBruijnAlgebra (Int -> Int -> DeBruijnExpr)
rebindAlgebra = ( \n     i j -> either (\x -> if (x<j {-local-}) then (Var n) else (Var (Left (x+i))))
                                     (\_ -> Var n)
                                     n
                , \g h   i j -> Appl (g i j) (h i j)
                , \n t h i j -> Lambda n t (h i (j+1))
                )
                      
-- de Bruijn indices

toDeBruijnExpression :: NamedExpr -> DeBruijnExpr
toDeBruijnExpression e = foldExpr toDeBruijnAlgebra e []

toDeBruijnAlgebra :: NamedAlgebra (Env -> DeBruijnExpr)
toDeBruijnAlgebra = ( v, a, l )
        where v n     env = Var ( maybe (Right n) (Left) (lookup n env) )
              a e f   env = Appl (e env) (f env)
              l n t e env = Lambda n t (e updateenv) where
                      updateenv = (n,0):(map (\(a,b)->(a,b+1)) env)

toNamedExpression :: DeBruijnExpr -> NamedExpr
toNamedExpression e = foldExpr toNamedExpressionAlgebra e [] (free' e)

toNamedExpressionAlgebra :: DeBruijnAlgebra ([Var] -> [Var] -> NamedExpr)
toNamedExpressionAlgebra = ( v, a, l )
        where v n   env free = Var (either (\x->(env!!x){-++"@"++(show x)-}) id n)
              a e f env free = Appl (e env free) (f env free)
              l n t e env free = Lambda n' t (e updateenv free) where
                        updateenv = n':env
                        n'        = if (n `elem` free) then n'' else (n)
                        n''       = n ++ show (until (\i->(n++(show i)) `notElem` free) (+1) 1)

