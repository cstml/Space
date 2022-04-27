module Space.Parser.Token where

import Control.Monad

import Text.Megaparsec
import Data.Text
import Data.String

import Data.Void

import Text.Megaparsec qualified as P
import Text.Megaparsec.Char.Lexer qualified as P
import Text.Megaparsec.Char qualified as P


type Parser =  Parsec Void Text

spaces_ :: Parser ()
spaces_ = void $ P.choice [many (P.char ' ')]

