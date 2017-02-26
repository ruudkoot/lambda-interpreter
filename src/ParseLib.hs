{-
This module is to be used for demonstration purposes only. For real applications you may consider the
uu-parsinglib package available form HackageDB
-}

module ParseLib where
import Char
-- changed so all parsers start with lower case p...

infixl 6  <*>, <$>, <*, *>, <$
infixr 4  <|>, `option`

-- The type of parsers

type Parser symbol result  =  [symbol] -> [(result,[symbol])]

-- Elementary parsers

-- in dictaat: symbol

pSymbol  ::  Eq s  =>  s -> Parser s s
pSymbol a []                  =  []
pSymbol a (x:xs) | x == a     =  [(x,xs)]
                 | otherwise  =  []

pSatisfy  ::  (s -> Bool) -> Parser s s
pSatisfy p []                 =  []
pSatisfy p (x:xs) | p x       =  [(x,xs)]
                  | otherwise =  []

-- in dictaat: tok

pToken  ::  Eq s => [s] -> Parser s [s]
pToken k xs  |  k == take n xs  =  [(k,drop n xs)]
             |  otherwise       =  []
  where  n = length k

pFailp     :: Parser s a
pFailp xs  =  []

pSucceed       :: a -> Parser s a
pSucceed r xs  =  [(r,xs)]

-- Parser combinators 

(<|>)         :: Parser s a      -> Parser s a -> Parser s a
(p <|> q) xs  =  p xs ++ q xs

(<*>)         :: Parser s (b -> a) -> Parser s b -> Parser s a
(p <*> q) xs  =  [(f x,zs)
                 |(f  ,ys) <- p xs
                 ,(  x,zs) <- q ys
                 ]

(<$>)         :: (a -> b) -> Parser s a -> Parser s b
f <$> p       =  pSucceed f <*> p

-- Common situations 

p <* q    = const <$> p <*> q

p *> q    = flip const <$> p <*> q

f <$ p    = const f <$> p


-- Applications of elementary parsers

pDigit  :: Parser Char Char
pDigit  =  pSatisfy (\x -> ord '0' <= ord x && ord x <= ord '9')

pDigAsInt  :: Parser Char Int
pDigAsInt  =  f <$> pDigit
  where  f c = ord c - ord '0'

-- EBNF parser combinators 

option      :: Parser s a -> a -> Parser s a
option p d  =  p <|> pSucceed d

pMany    :: Parser s a  -> Parser s [a]
pMany p  =  (:) <$> p <*> pMany p <|> pSucceed []

pMany1    :: Parser s a -> Parser s [a]
pMany1 p  =  (:) <$> p <*> pMany p

pPack        :: Parser s a -> Parser s b -> Parser s c -> Parser s b
pPack p r q  =  p *> r <* q 

pListSep      :: Parser s a -> Parser s b -> Parser s [a]
pListSep p s  =  (:) <$> p <*> pMany ( s *> p)

-- Auxiliary functions

determ  ::  Parser s b -> Parser s b
determ p xs  |  null r     =  []
             |  otherwise  =  [head r]
  where r = p xs

greedy, greedy1  ::  Parser s b -> Parser s [b]
greedy   =  determ . pMany
greedy1  =  determ . pMany1

-- Applications of EBNF combinators

pNatural  :: Parser Char Int
pNatural  =  foldl (\a b -> a*10 + b) 0 <$> pMany1 pDigAsInt

pInteger  ::  Parser Char Int
pInteger  =  (negate <$ (pSymbol '-') `option` id ) <*>  pNatural 

pIdentifier :: Parser Char String
pIdentifier =  (:) <$> pSatisfy isAlpha <*> greedy (pSatisfy isAlphaNum)

pParens p  =  pPack (pSymbol '(') p (pSymbol ')')

pCommaList    :: Parser Char a -> Parser Char [a]
pCommaList p  =  pListSep p (pSymbol ',')

pSequence         :: [Parser s a] -> Parser s [a]
pSequence []      =  pSucceed []
pSequence (p:ps)  =  (:) <$> p <*> pSequence ps


choice  :: [Parser s a] -> Parser s a
choice  =  foldr (<|>) pFailp
