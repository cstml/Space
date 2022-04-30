module Main where

import Control.Lens
import Control.Monad
import Prettyprinter
import Space
import Space.Interpreter
import Space.Evaluator.Implementation.Pure as Pure

main :: IO ()
main = do
  (either (error.show) id . parseTerm -> t) <- getLine
  let m = either (error.show) id $ eval mempty t
  print m
   

