module Space.Parser (module X) where

import Space.Parser.Term as X
import Space.Parser.Token as X

import Space.Parser.Token
import Space.Language

import Data.Text qualified as T
import Data.String

import Control.Applicative
import Control.Monad

import Text.Megaparsec qualified as P
import Text.Megaparsec.Char.Lexer qualified as P
import Text.Megaparsec.Char qualified as P


parseTerm :: forall e. String -> Either e Term
parseTerm = undefined
