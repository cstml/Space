{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Space.Syntax.Parser where

import qualified Text.Parsec.Token as T

import Space.Syntax.Types
import Space.TypeChecking.Context

import Text.Parsec.Language
import Data.Functor
import Text.Parsec
import Data.Bifunctor

type Parser = Parsec String ()

parseDefinitions :: String -> Either ParseError (Term,Context)
parseDefinitions = runParser pDefinitions () "stdInput"

pDefinitions :: Parser (Term,Context)
pDefinitions =   go <$> pExpressions
  where
    go :: [Expression] ->(Term,Context)
    go [] = (mempty, mempty)
    go (x:xs) = case x of
      TermExp term -> first (term <>) (go xs)
      TypeExp typ -> second (Context [typ] [] <>) (go xs)

pExpressions :: Parser [Expression]
pExpressions = many (whiteSpace >> pExpression)

pExpression :: Parser Expression
pExpression = choice [ TypeExp <$> pTypeBinding
                     , TermExp <$> pTerm
                     ]

pTypeDef :: Parser Context
pTypeDef = do
  b <- pTypeBinding
  pure $ Context [b] []

parseTerm :: String -> Either ParseError Term
parseTerm = runParser pTerm () "stdInput"

pTypeBinding :: Parser (Variable, TermType)
pTypeBinding = do
  whiteSpace >>  reserved "type" >>  whiteSpace
  v <- pVar
  whiteSpace >> char ':' >> whiteSpace
  t <- pType <* whiteSpace
  return (v,t)

pTerm :: Parser Term
pTerm = do
  whiteSpace
  chainr1 (choice $ lexeme <$> [ pClosure, pVariable, pBind , pPush]) (pure (<>))

pPush :: Parser Term
pPush = do
  t <- brackets pTerm
  l <- pLocation
  pure $ Push l t NoOp

pBind :: Parser Term
pBind = do
  v <- angles pVar
  l <- pLocation
  pure $ Bind l v NoOp

pClosure :: Parser Term
pClosure = Closure <$> braces pTerm <*> pure NoOp

pVariable :: Parser Term
pVariable = Variable <$> pVar <*> pure NoOp

pVar :: Parser Variable
pVar = MkVariable <$> identifier

pAtom :: Parser Atom
pAtom = MkAtom <$> identifier

pNoOp :: Parser Term
pNoOp = whiteSpace $> NoOp

pLocation :: Parser Location
pLocation = do
  symbol "@"
  choice [ symbol "_" $> Spine
         , symbol "i" $> Input
         , symbol "o" $> Output
         , symbol "^" $> Return
         ]

spaceDef :: LanguageDef st
spaceDef = emptyDef
  { T.commentStart = "{-"
  , T.commentEnd = "-}"
  , T.commentLine = "--"
  , T.nestedComments = True
  , T.caseSensitive = False
  , T.identStart = alphaNum <|> oneOf "-+*%/"
  , T.identLetter = alphaNum <|> oneOf "_'"
  , T.opStart = T.opLetter spaceDef
  , T.opLetter = oneOf "@_"
  , T.reservedNames = ["type"]
  }

-- | A lexer for Space.
sL :: T.TokenParser st
sL = T.makeTokenParser spaceDef

lexeme = T.lexeme sL
angles = T.angles sL
identifier = T.identifier sL
braces = T.braces sL
brackets = T.brackets sL
parens = T.parens sL
symbol = T.symbol sL
whiteSpace = T.whiteSpace sL
reserved = T.reserved sL

pType :: Parser TermType
pType = choice
  [ pArrow
  , TypeVar <$> pAtom
  , pTVector
  ]

pTVector :: Parser TermType
pTVector = do
  v <- TypeVector <$> braces (pType `sepBy` char ',')
  l <- pLocation
  pure $ TypeLocation l v

pArrow :: Parser TermType
pArrow = do
  types <- parens (pType `sepBy` (whiteSpace >> string "->" <* whiteSpace))
  pure $ foldr1 (:->:) types

