module Space.Test.Parser (tParser) where

import Test.Tasty
import Space.Syntax.Parser
import Space.Syntax.Types
import Test.Tasty.HUnit

tParser :: TestTree
tParser = testGroup "Parser Tests"
  [ t1 "x" (Right (Variable "x" NoOp))
  , t1 "x y { x }" (Right (Variable "x" NoOp))
  ]
  where
    t1 s r =  testCase s $ parse s @?= r
