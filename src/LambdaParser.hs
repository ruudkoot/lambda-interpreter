{-# LANGUAGE TypeSynonymInstances #-}

module LambdaParser where


import Char
import LambdaData
import qualified Data.Map as M
import Data.Map (Map(..))  
import System.IO


newtype Parser t = P (String -> [(t, String)])
parse :: Parser t -> ( String -> [(t,String)])
parse (P p) = p


-- ---------------------------- --
-- Parser is a Monad		--
-- ---------------------------- --
instance Monad Parser where
	p >>= f = P ( \inp -> case parse p inp of
				[] -> []
				[(v,out)] -> parse (f v) out
		    )
	return v = P ( \inp -> [(v,inp)])

-- ---------------------------- --
-- Choice operator		--
-- ---------------------------- --
(+++) :: Parser a -> Parser a -> Parser a
p +++ q = P ( \inp -> case parse p inp of
		[ ] -> parse q inp
		[(v, out)] -> [(v, out)]
	    )


-- ----------------------------	--
-- Applies a parser (0 or)	--
-- many times until it fails	--
-- ---------------------------- --
many0 :: Parser a -> Parser [a]
many0 p = many p +++ return []

many :: Parser a -> Parser [a]
many p = do 
		v <- p
		vs <- many0 p
		return (v:vs)

-- ------------------------------------ --
-- A parser that accepts any char  	--
-- ------------------------------------ --
item :: Parser Char
item = P ( \inp -> case inp of
			[] -> []
			(x:xs) -> [(x,xs)]
	 )

-- ------------------------------------ --
-- A parser that accepts any char that 	--
-- satisfies p				--
-- ------------------------------------ --
sat :: (Char -> Bool) -> Parser Char
sat p = do 	x <- item
	 	if p x then return x else failure

-- ------------------------------------ --
-- A parser that accepts no char  	--
-- ------------------------------------ --
failure :: Parser a
failure = P (\inp ->  [])



-- ------------------------------------ --
-- A parser that return a char without 	--
-- consuming it				--
-- ------------------------------------ --
lookahead :: Parser Char
lookahead = P ( \inp -> case inp of
			[] 	-> []
			(x:xs)  -> [(x, x:xs)]  )


-- ------------------------------------------------------------ --
-- Function that generates a parser that recognizes the string	--
-- given as parameter						--
-- ------------------------------------------------------------ --
match :: String -> Parser String
match str = P ( \inp -> let 	lg = length str in
				[(str, drop lg inp) | take lg inp == str]
		) 
-- ------------------------------------------------------------ --
-- Parser that consumes any number of white spaces it meets 	--
-- ------------------------------------------------------------ --
iSp :: Parser String
iSp = many0 (sat isSpace) 




-- ************************************************************ --
-- 								--
--    HERE ARE THE PARSERS FOR EACH NONTERMINAL IN OUR GRAMMAR 	--
--								--
-- ************************************************************	--




-- -------------------------------------------- --
-- Recognizes a variable and packs it in the	--
-- given constructor 				--
-- --------------------------------------------	--
variable :: (String -> a) -> Parser a
variable f = do
		iSp;
		x  <- (sat isAlphaNum);
		xs <- many (sat isAlphaNum) +++ return [];
		return (f (x:xs))

-- -------------------------------------------- --
-- Parser that recognizes a typed variable 	--
--  ( v : t) 					--
-- -------------------------------------------- --
typedVariable :: Parser TypedVariable
typedVariable = do 	iSp >> match "("
		 	v <- (iSp >> variable id)	
			iSp >> match ":"
			t <- typeExp
			iSp >> match ")"
			return ( v, t)

-- -------------------------------------------- --
-- Parser that recognizes a type expression for --
-- simply typed lambda calculus			--
-- -------------------------------------------- --
typeExp :: Parser Type
typeExp = do 	iSp; 
		t1 <- typeNonFunc
		ts <- (many0 ( iSp >> match "->" >> typeNonFunc)) 
		let types = reverse (t1:ts) 
		return ( foldl  (\x y -> Function y x)  (head types) (tail types))


-- -------------------------------------------- --
-- Recognizes a type that is not function	--
-- -------------------------------------------- --
typeNonFunc :: Parser Type
typeNonFunc = do
		iSp;
		variable Type +++ parantSmth typeExp
		
		
	     

-- ------------------------------- --
-- Recognizes a lambda abstraction --
-- ------------------------------- --
lambdaAbstraction :: Parser Expr
lambdaAbstraction = do 	iSp >> match "\\";
			vs <- many (iSp >> typedVariable);
			iSp >> match "->";
			b <- lambdaExp 
			return ( foldr (uncurry Lambda) b vs) 

-- ------------------------------------ --
-- Recognizes a lambda aplication	--
-- ------------------------------------ --
lambdaAplication :: Parser Expr
lambdaAplication = do 	iSp
			e1 <- exprNonApp
			e2 <- exprNonApp
			es <- many0 exprNonApp
			return ( foldl Appl (Appl e1 e2) es)
-- -------------------------------------------- --
-- Recognizes a paranthesized expression 	--
-- -------------------------------------------- --
parantSmth :: Parser a -> Parser a
parantSmth p = do 
		iSp >> match "("
		e <- p
		iSp >> match ")"
		return e

-- ------------------------ --
-- Recognizes an expression --
-- ------------------------ --
lambdaExp :: Parser Expr
lambdaExp = do 	iSp
		lambdaAplication +++ exprNonApp

-- ------------------------------------ --
-- Recongnizes an expression which is 	--
-- not function application		--
-- -----------------------------------  --
exprNonApp :: Parser Expr
exprNonApp = do iSp
		lambdaAbstraction +++ parantSmth lambdaExp +++ variable Var
-- ---------------------------- --
-- Recognizes a binding inside 	--
-- a let declaration		--
-- ---------------------------- --
statement :: Parser (String, Expr)
statement = do
		v <- iSp >> variable id;
		iSp >> match "=";
		e <- lambdaExp;
		iSp >> match ";"
		return ( v,e)

-- ---------------------------- --
-- Recognizes a commnet 	--
-- ---------------------------- --
comment :: Parser String
comment = do 
		many0 (sat (\x -> isSpace x && x /= '\n'))
		many0 (sat (\x -> x == '\n'))
		match "--";
		many0 (sat (\x -> x /= '\n'))

-- ----------------------------	--
-- Recognizes a let expression 	--
-- ---------------------------- --	
letExpr :: Parser ParsedLet
letExpr = do
		iSp >> match "let";
		stmts <- many0 ( many0 comment >>  statement)  
		iSp >> match "in"
		es   <- many ( many0 comment >> ( do e <- lambdaExp; match ";"; return e; )) 
		iSp;
		return ( stmts , es)




-- ----------------------------	--
-- Returns the structure of a	--
-- .la parsed file		--
-- ---------------------------- --
lambdaFile :: Parser ParsedFile
lambdaFile  = do 
		( l , stmts) <- letExpr
		return ( M.fromList l, stmts)





