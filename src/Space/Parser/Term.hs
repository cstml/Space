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

-- | Term parser - parses any Space term.
pTerm :: Parser Term
pTerm =
  P.try pEmptyTerm
    <|> ( P.choice . fmap (P.try . pTermWithInfer) $
            [pChar, pInteger, pPushDef, pPush, pPopDef, pPop, pVariable]
        )

-- | Location parser.
--
-- Locations can be arbitrary or the default location.
pLocation :: Parser Location
pLocation = P.try pLocationArbitrary <|> P.try pLocationDefault

-- | Default location.
--
-- The default location is either a simple @ or it can be ommitted alltogether
-- from the term
--
-- Example:
--
-- >>> pTest parser string = P.parse parser "" (fromString string)
--
-- >>> pTest pLocationDefault "@"
-- Right DLocation
pLocationDefault :: Parser Location
pLocationDefault = lex_ (P.char '@') >> pure DLocation

-- | Arbitrary Location parser.
--
-- Arbitrary locations are strings of the form:
-- @UpperChar+ {UpperChar | LowerChar}*
--
-- Example:
--
-- >>> pTest parser string = P.parse parser "" (fromString string)
-- >>> pTest pLocationArbitrary "@In"
-- Right (Location "In")
-- >>> pTest pLocationArbitrary "@Out"
-- Right (Location "Out")
-- >>> pTest pLocationArbitrary "@I"
-- Right (Location "I")
pLocationArbitrary :: Parser Location
pLocationArbitrary = lex_ $ do
  _ <- P.char '@'
  a <- P.upperChar
  b <- P.many P.alphaNumChar
  pure . Location $ a : b

pEmptyTerm :: Parser Term
pEmptyTerm = lex_ . P.try $ P.char '{' >> P.char '}' >> return SEmpty

-- | Empty Term Infering parser.
--
-- It is a parser that returns a star without parsing any character.
pEmptyTermInfer :: Parser Term
pEmptyTermInfer = P.try $ P.lookAhead (lex_ $ P.char ']') >> return SEmpty

-- | Infer that the term has finalised as parsing reached EOF.
pEOFInfer :: Parser Term
pEOFInfer = lex_ P.eof >> return SEmpty

-- | Variable Parser.
pVar :: Parser Variable
pVar = fmap Variable . lex_ $ P.choice [atom, operation]
 where
  atom = do
    x <- P.letterChar
    xs <- P.many P.letterChar
    pure $ x : xs

  operation = T.unpack <$> (P.choice . fmap (P.string . fromString) $ ["==", "/=", "+", "-", "/", "*", "!", "?"])

-- | Infer an empty term.
pTermWithInfer :: Parser (Term -> Term) -> Parser Term
pTermWithInfer p = do
  v <- p
  P.choice $
    fmap
      P.try
      [ lex_ pSeparator >> v <$> pTerm
      , lex_ pSeparator >> v <$> pEmptyTermInfer
      , lex_ pSeparator >> v <$> pEOFInfer
      , v <$> pEmptyTermInfer
      , v <$> pEOFInfer
      , space_ >> pTerm >>= (\x -> return $ x <> v SEmpty)
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

pPopDef :: Parser (Term -> Term)
pPopDef = flip SPop DLocation <$> P.between (lex_ $ P.char '<') (lex_ $ P.char '>') pVar

pPop :: Parser (Term -> Term)
pPop = flip SPop <$> pLocation <*> P.between (lex_ $ P.char '<') (lex_ $ P.char '>') pVar

pSeparator :: Parser ()
pSeparator = lex_ . void $ P.choice $ P.try <$> [P.char ';']

pComposing :: Parser ()
pComposing = lex_ . void $ P.choice $ P.try <$> [P.char '.']
