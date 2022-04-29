module Test.Space.Language (test) where

import Control.Lens
import Data.Map qualified as M
import Data.Sequence qualified as S
import Data.String
import Data.Text qualified as T
import Space
import Space.Interface.REPL
import Space.Language qualified as L
import Space.Parser.Term as X
import Space.Parser.Token as X
import Test.Tasty
import Test.Tasty.HUnit
import Test.HUnit


-- FIXME
test = testGroup "Language Unit Tests."
        [ 1 ~=? 1
        ]
