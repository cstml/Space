{-# LANGUAGE OverloadedStrings   #-}

module Space.Parser.Term where

import Space.Parser.Token
import Space.Language.Term

import Data.Text qualified as T
import Data.String

import Control.Applicative
import Control.Monad

import Text.Megaparsec qualified as P
import Text.Megaparsec.Char.Lexer qualified as P
import Text.Megaparsec.Char qualified as P

term :: Parser Term
term = P.choice
  [ emptyTerm
  ]

emptyTerm :: Parser Term
emptyTerm = P.char '*' >> return SEmpty




