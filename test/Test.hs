module Main where

import Test.Space.Evaluator.Machine qualified as Machine
import Test.Space.Parser qualified as Parser
import Test.Space.REPL qualified as REPL
import Test.Tasty

main :: IO ()
main = defaultMain allTests

allTests = testGroup "AllTests" [Machine.test1, Parser.unit]
