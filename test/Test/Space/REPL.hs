module Test.Space.REPL (test) where

import Control.Lens
import Data.Map qualified as M
import Data.Sequence qualified as S
import Data.String
import Data.Text qualified as T
import Space
--import Space.Interface.REPL
import Space.Language qualified as L
import Space.Parser.Term as X
import Space.Parser.Token as X
import Test.Tasty
import Test.Tasty.HUnit
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Text.Megaparsec.Char.Lexer qualified as P

-- FIXME
test = testGroup "REPL Unit Tests." []
