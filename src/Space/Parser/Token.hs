{-# LANGUAGE OverloadedStrings #-}

module Space.Parser.Token where

import Control.Monad
import Data.String
import Data.Text
import Data.Void
import Text.Megaparsec
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Text.Megaparsec.Char.Lexer qualified as L
import Space.Parser.Util (Parser)

space_ :: Parser ()
space_ =
  L.space
    P.space1 -- space
    (L.skipLineComment "-- ") -- Line Comment
    (L.skipBlockCommentNested "{-" "-}") -- block comments

lex_ :: forall a. Parser a -> Parser a
lex_ = L.lexeme space_
