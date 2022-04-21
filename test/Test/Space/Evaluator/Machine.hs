module Test.Space.Evaluator.Machine where

import Space
import Space.Evaluator.Machine
import Test.Tasty
import Test.Tasty.HUnit

test1 =
  testGroup
    "Evaluator Machin Tests"
    [ testCase "2+2=4" $
        2 + 2 @?= 4
    , testCase "Operations" $
        assertEqual "Push is Working" (evaluate' (SInteger 3)) mempty
    ]
