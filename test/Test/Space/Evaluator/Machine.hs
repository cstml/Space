module Test.Space.Evaluator.Machine where

import Space
import Space.Language
import Space.Evaluator
import Test.Tasty
import Test.Tasty.HUnit
import Control.Lens
import qualified Data.Sequence as S
import qualified Data.Map as M
test1 =
  testGroup
    "Evaluator Machin Tests"
    [ testCase "2+2=4" $
        2 + 2 @?= 4
    , testCase "Operations" $
        assertEqual "Push is Working"
        (evaluate' [(SInteger 3),(SInteger 3)]) 
        ( stacks .= M.fromList [("Ho", stack .= S.fromList [SInteger 3,SInteger 3] $ mempty )]
          . (binds .= M.fromList [] $ mempty)
        )
    ]
