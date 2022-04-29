{-# LANGUAGE OverloadedStrings #-}

module Space.Parser.Term where

import Control.Applicative
import Control.Monad
import Data.String
import Data.Text qualified as T
import Space.Language.Location
import Space.Language.Term
import Space.Language.Variable
import Space.Parser.Token
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Text.Megaparsec.Char.Lexer qualified as P

pTerm :: Parser Term
pTerm =
  lex_ . P.choice $
    P.try
      <$> [ pChar
          , pInteger
          , pVariable
          , pPushHo
          , pPush
          , pPopHo
          , pPop
          , pEmptyTerm
          ]

-- |
pTerm' = pTerm <|> pure SEmpty

pLocation :: Parser Location
pLocation = lex_ $ do
  _ <- P.char '@'
  a <- P.upperChar
  b <- P.alphaNumChar
  pure . Location $ [a, b]

pEmptyTerm :: Parser Term
pEmptyTerm = P.char '*' >> return SEmpty

pVar :: Parser Variable
pVar = Variable . pure <$> lex_ P.lowerChar

pVariable :: Parser Term
pVariable = do
  (SVariable . Variable . pure -> v) <- lex_ P.printChar
  pSeparator
  v <$> pTerm'

pChar :: Parser Term
pChar = do
  (SChar -> c) <- lex_ $ P.char '\'' >> P.letterChar <* P.char '\''
  pSeparator
  c <$> pTerm'

pInteger :: Parser Term
pInteger = do
  (SInteger -> i) <- lex_ P.decimal
  pSeparator
  i <$> pTerm'

pPushHo :: Parser Term
pPushHo = do
  (\x -> SPush x (Location "Ho") -> p) <- P.between (lex_ $ P.char '[') (lex_ $ P.char ']') pTerm
  pSeparator
  p <$> pTerm'

pPush :: Parser Term
pPush = do
  l <- pLocation
  ((`SPush` l) -> p) <- P.between (lex_ $ P.char '[') (lex_ $ P.char ']') pTerm
  pSeparator
  p <$> pTerm'

pPopHo :: Parser Term
pPopHo = do
  (\x -> SPop x (Location "Ho") -> p) <- P.between (lex_ $ P.char '<') (lex_ $ P.char '>') pVar
  pSeparator
  p <$> pTerm'

pPop :: Parser Term
pPop = do
  l <- pLocation
  ((`SPop` l) -> p) <- P.between (lex_ $ P.char '<') (lex_ $ P.char '>') pVar
  pSeparator
  p <$> pTerm'

pSeparator :: Parser ()
pSeparator = lex_ . void $ P.choice $ P.try <$> [P.char ';', P.char ' ']
