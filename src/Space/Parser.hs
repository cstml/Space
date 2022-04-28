module Space.Parser (
  module X,
  parseTerm,
) where

import Control.Applicative
import Control.Monad
import Data.String
import Data.Text qualified as T
import Data.Void
import Space.Language qualified as L
import Space.Parser.Term as X
import Space.Parser.Token as X
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Text.Megaparsec.Char.Lexer qualified as P

testParseTerm :: String -> Maybe L.Term
testParseTerm = P.parseMaybe pTerm . fromString

parseTerm :: String -> Either (P.ParseErrorBundle T.Text Void) L.Term
parseTerm = P.parse pTerm "Standard Input" . T.pack
