module Test.Space.Parser where

import Control.Lens
import Data.Map qualified as M
import Data.Sequence qualified as S
import Space
import Test.Tasty
import Test.Tasty.HUnit

ok :: forall a b. a -> Either b a
ok = pure

unit = testGroup "Parser Unit Tests"
       [ testCase "Parse empty Term" $
           assertEqual
             "Working Push Int 3s."
             (ok SEmpty )
             (parseTerm "*")
       ]
