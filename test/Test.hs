module Main where

import Test.Space.Evaluator.Machine qualified as Machine
import Test.Tasty

main :: IO ()
main = defaultMain allTests

allTests = testGroup "AllTests" [Machine.test1]