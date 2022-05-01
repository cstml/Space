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
      <$> [ pEmptyTerm
          , pChar
          , pChar'
          , pInteger
          , pInteger'
          , pVariable
          , pVariable'
          , pPushDef
          , pPushDef'
          , pPush
          , pPush'
          , pPopHo
          , pPopHo'
          , pPop
          , pPop'
          ]

pTerm1 =
  lex_ . P.choice $
    P.try <$> [ pVariable1 ]
      
pTerm' = pTerm <|> pImpliedStar

pLocation :: Parser Location
pLocation = lex_ $ do
  _ <- P.char '@'
  a <- P.upperChar
  b <- P.alphaNumChar
  pure . Location $ [a, b]

pEmptyTerm :: Parser Term
pEmptyTerm = P.char '*' >> return SEmpty

pEmptyTerm1 :: Parser Term
pEmptyTerm1 = return SEmpty

pVar :: Parser Variable
pVar = Variable . pure <$> lex_ P.lowerChar

pImpliedStar :: Parser Term
pImpliedStar = do
  P.choice $ P.try <$> [ void P.eol , P.eof , space_]
  pure SEmpty

pVariable :: Parser Term
pVariable = do
  (SVariable . Variable . pure -> v) <- lex_ P.printChar
  pSeparator
  v <$> pTerm'

pVariable' :: Parser Term
pVariable' = do
  (SVariable . Variable . pure -> v) <- lex_ P.printChar
  _ <- void P.eol <|> P.eof
  pure $ v SEmpty

pVariable'' :: Parser Term
pVariable'' = do
  (SVariable . Variable . pure -> v) <- lex_ P.printChar
  pure $ v SEmpty

pVariable1 :: Parser Term
pVariable1 = do
  (SVariable . Variable . pure -> v) <- lex_ P.printChar
  pure $ v SEmpty

pChar :: Parser Term
pChar = do
  (SChar -> c) <- lex_ $ P.char '\'' >> P.letterChar <* P.char '\''
  pSeparator
  c <$> pTerm'

pChar' :: Parser Term
pChar' = do
  (SChar -> c) <- lex_ $ P.char '\'' >> P.letterChar <* P.char '\''
  c <$> pImpliedStar

pInteger :: Parser Term
pInteger = do
  (SInteger -> i) <- lex_ P.decimal
  pSeparator
  i <$> pTerm'

pInteger' :: Parser Term
pInteger' = do
  (SInteger -> i) <- lex_ P.decimal
  _ <- void P.eol <|> P.eof
  i <$> pImpliedStar

pPushDef :: Parser Term
pPushDef = do
  (flip SPush DLocation -> p) <- P.between (lex_ $ P.char '[') (lex_ $ P.char ']') pTerm'
  pSeparator
  p <$> pTerm'

pPushDef' :: Parser Term
pPushDef' = do
  (flip SPush DLocation -> p) <- P.between (lex_ $ P.char '[') (lex_ $ P.char ']') pTerm'
  p <$> pImpliedStar

pPush :: Parser Term
pPush = do
  l <- pLocation
  ((`SPush` l) -> p) <- P.between (lex_ $ P.char '[') (lex_ $ P.char ']') pTerm'
  pSeparator
  p <$> pTerm'

pPush' :: Parser Term
pPush' = do
  l <- pLocation
  ((`SPush` l) -> p) <- P.between (lex_ $ P.char '[') (lex_ $ P.char ']') pTerm'
  p <$> pImpliedStar

pPopHo :: Parser Term
pPopHo = do
  (\x -> SPop x (Location "Ho") -> p) <- P.between (lex_ $ P.char '<') (lex_ $ P.char '>') pVar
  pSeparator
  p <$> pTerm'

pPopHo' :: Parser Term
pPopHo' = do
  (\x -> SPop x (Location "Ho") -> p) <- P.between (lex_ $ P.char '<') (lex_ $ P.char '>') pVar
  p <$> pImpliedStar

pPop :: Parser Term
pPop = do
  l <- pLocation
  ((`SPop` l) -> p) <- P.between (lex_ $ P.char '<') (lex_ $ P.char '>') pVar
  pSeparator
  p <$> pTerm'

pPop' :: Parser Term
pPop' = do
  l <- pLocation
  ((`SPop` l) -> p) <- P.between (lex_ $ P.char '<') (lex_ $ P.char '>') pVar
  p <$> pImpliedStar

pSeparator :: Parser ()
pSeparator = lex_ . void $ P.choice $ P.try <$> [P.char ';', P.char ' ']
