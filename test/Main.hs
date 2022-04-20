module Main where

import Test.Tasty
import qualified Test.Space.Evaluator.Machine as Machine

main :: IO ()
main = defaultMain allTests

allTests = testGroup "AllTests" [Machine.test1]