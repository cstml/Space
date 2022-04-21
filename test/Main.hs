module Main where

import qualified Test.Space.Evaluator.Machine as Machine
import Test.Tasty

main :: IO ()
main = defaultMain allTests

allTests = testGroup "AllTests" [Machine.test1]
