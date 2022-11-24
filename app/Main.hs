module Main where

import Space.Evaluator
import Space.Syntax.Parser
import Prettyprinter

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  content <- readFile "test/Examples/example-terms.sp"
  case parseTerm content of
    Left e -> error $ show e 
    Right term ->   do
      res <- evalTerm term
      case res of 
        Right e ->  (print . pretty) e
        Left e -> print e
