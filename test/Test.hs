module Main (main) where

import Test.Tasty
import Space.Test.Parser
import Space.Test.Evaluator

main :: IO ()
main = defaultMain $ adjustOption (const $ mkTimeout 1_000_000) tests

tests :: TestTree
tests = testGroup "Tests" [tParser, tEvaluator]
