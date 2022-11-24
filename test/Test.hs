module Main (main) where

import Test.Tasty
import Space.Test.Parser

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [tParser]
