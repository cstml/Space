module Main where

import Space.Evaluator
import Space.Syntax.Parser
import Space.Syntax.Types
import Prettyprinter

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  repl (mempty,mempty)

repl :: (Bindings, Stacks) -> IO ()
repl mem = do 
  content <- getLine
  case parseTerm content of
      Left e -> do 
        print e 
        repl mem 
      Right term -> do
        computationResult <- runEval' mem term
        case computationResult of 
          Left e -> print e >> repl mem
          Right res@(_,newMem) -> do (print . pretty) res >> repl newMem
