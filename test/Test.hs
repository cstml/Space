module Main where

import Test.Space.Evaluator.Machine qualified as Machine
import Test.Space.Language qualified as Language
import Test.Space.Parser qualified as Parser
import Test.Space.REPL qualified as REPL
import Test.Space.TypeChecker.Derive qualified as TypeChecker
import Test.Tasty

main :: IO ()
main = defaultMain allTests

allTests =
  testGroup
    "Space Test Suite."
    [ Machine.test1
    , Parser.unit
    , REPL.test
    , Language.test
    , TypeChecker.test
    ]
