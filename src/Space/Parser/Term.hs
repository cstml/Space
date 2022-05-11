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
  P.try pEmptyTerm
    <|> ( P.choice . fmap (P.try . pTermWithInfer) $
            [pChar, pInteger, pPushDef, pPush, pPopHo, pPop, pVariable]
        )

pLocation :: Parser Location
pLocation = lex_ $ do
  _ <- P.char '@'
  a <- P.upperChar
  b <- P.alphaNumChar
  pure . Location $ [a, b]

pEmptyTerm :: Parser Term
pEmptyTerm = lex_ . P.try $ P.char '*' >> return SEmpty

pEmptyTermInfer :: Parser Term
pEmptyTermInfer = return SEmpty

pVar :: Parser Variable
pVar = Variable . pure <$> lex_ P.printChar

pTermWithInfer :: Parser (Term -> Term) -> Parser Term
pTermWithInfer p = do
  v <- p
  P.choice $
    fmap
      P.try
      [ lex_ pSeparator >> v <$> pTerm
      , lex_ pSeparator >> v <$> pEmptyTermInfer
      , v <$> pEmptyTermInfer
      ]

pVariable :: Parser (Term -> Term)
pVariable = SVariable <$> lex_ pVar

pChar :: Parser (Term -> Term)
pChar = SChar <$> P.between (lex_ $ P.char '\'') (lex_ $ P.char '\'') P.letterChar

pInteger :: Parser (Term -> Term)
pInteger = SInteger <$> lex_ P.decimal

pPushDef :: Parser (Term -> Term)
pPushDef = (`SPush` DLocation) <$> P.between (lex_ $ P.char '[') (lex_ $ P.char ']') pTerm

pPush :: Parser (Term -> Term)
pPush = flip SPush <$> pLocation <*> P.between (lex_ $ P.char '[') (lex_ $ P.char ']') pTerm

pPopHo :: Parser (Term -> Term)
pPopHo = flip SPop (Location "Ho") <$> P.between (lex_ $ P.char '<') (lex_ $ P.char '>') pVar

pPop :: Parser (Term -> Term)
pPop = flip SPop <$> pLocation <*> P.between (lex_ $ P.char '<') (lex_ $ P.char '>') pVar

pSeparator :: Parser ()
pSeparator = lex_ . void $ P.choice $ P.try <$> [P.char ';', P.char ' ']
