module Main where

import Prelude hiding (lookup)

import Control.Monad.State
import Data.Char
import Data.Map hiding (map,null)
import System.Exit
import System.IO

import LambdaData
import LambdaInference
import LambdaInterpreter
import LambdaParser
import LambdaUtil

type InterpreterState = Map String Expr

fullpars   = fst . head . parse lambdaExp
fulleval m = eval m . fullpars
fullsubst m = subst m . fullpars

main :: IO ()
main = do hSetBuffering stdout NoBuffering
          hSetBuffering stdin  LineBuffering
          putStr "ULIi, version 0.00.0: http://www.example.com/uli/  :? for no help\n"          
          runStateT interactive empty
          return ()

interactive :: StateT (Map String Expr) IO ()
interactive = do s <- get
                 liftIO $ putStr ((if (null (keys s)) then "" else "Prelude") ++ "> ")
                 i <- liftIO getLine
                 (id:ic:is,ie) <- return (splitAt 3 i)
                 case id of
                        ':' -> case ic of
                                '=' -> let (n,s) = splitOn isSpace ie
                                        in modify (insert n (fullpars s))
                                '/' -> let (n,s) = splitOn isSpace ie
                                        in modify (delete n)
                                '?' -> liftIO $ doHelp
                                'h' -> liftIO $ doHelp
                                'q' -> liftIO $ doQuit
                                'l' -> doLoad ie
                                'b' -> doBrowse
                                't' -> liftIO $ doType (fulleval s ie) (fullsubst s ie)
                                _   -> liftIO $ doUnrec
                        _   -> do e <- return $ fulleval s i
                                  liftIO $ putStr (show e ++ "\n")
                 interactive

doHelp :: IO ()
doHelp      = putStr "Here be dragons!\n"

doQuit :: IO ()
doQuit      = exitSuccess

doLoad :: String -> StateT InterpreterState IO ()
doLoad file = do txt <- liftIO $ readFile file
                 (prs,_) <- return $ fst (head (parse lambdaFile txt))
                 modify (`union` prs)
                 return ()

doBrowse :: StateT InterpreterState IO ()
doBrowse = do m <- get
              k <- return $ keys m
              liftIO $ sequence_ $ map (\x->putStrLn (show x ++ " :: " ++ (maybe "ERROR" (show) (inferType (maybe undefined id (lookup x m)))))) k

doType :: NamedExpr -> NamedExpr -> IO ()
doType e s = putStr (maybe ("Type error\n")
                         (\x->show e++" :: "++show x++"\n")
                         (inferType s)
                  )

doUnrec :: IO ()
doUnrec  = putStr "Unrecognized command\n"

