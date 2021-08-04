module Main where
import Parse

import System.IO (hFlush, stdout)

import Data.HashMap.Strict as H (HashMap, empty, fromList, insert, lookup, union)
import Data.Functor.Identity

import Text.ParserCombinators.Parsec hiding (Parser)
import Text.Parsec.Prim (ParsecT)

import Lib


--- REPL
--- ----

repl :: PEnv -> Env -> [String] -> String -> IO Result
repl penv env [] _ =
  do putStr "Python> "
     hFlush stdout
     input <- getLine
     case parse stmt "stdin" input of
        Right QuitStmt -> do putStrLn "Bye!"
                             return ("",penv,env)
        Right x -> let (nuresult,nupenv,nuenv) = exec x penv env
                   in do {
                     putStrLn nuresult;
                     repl nupenv nuenv [] "stdin"
                   }
        Left x -> do putStrLn $ show x
                     repl penv env [] "stdin"

main :: IO Result
main = do
  putStrLn "Welcome to Python-Haskell interpreter!"
  repl H.empty H.empty [] "stdin"

