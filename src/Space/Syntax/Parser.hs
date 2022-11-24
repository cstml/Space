{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Space.Syntax.Parser where

import qualified Text.Parsec.Token as T
import Space.Syntax.Types
import Text.Parsec.Language
import Data.Functor
import Text.Parsec 
import Text.Parsec.Char 

type Parser = Parsec String () 


parse :: String -> Either ParseError Term
parse str = runParser pTerm () "stdInput" str 

pTerm :: Parser Term
pTerm = chainr1 (choice $ lexeme <$> [ pClosure, pVariable, pBind ]) (pure (<>))  

pBind :: Parser Term
pBind = do
  v <- angles pAtom
  l <- pLocation
  pure $ Bind l v NoOp

pClosure :: Parser Term
pClosure = Closure <$> braces pTerm <*> pure NoOp

pVariable :: Parser Term
pVariable = Variable <$> pAtom <*> pure NoOp

pAtom :: Parser Variable
pAtom = MkVariable <$> identifier

pNoOp :: Parser Term
pNoOp = whiteSpace $> NoOp

-- pSpace :: Parser ()
-- pSpace = space space1 (skipLineComment   "--") (skipBlockCommentNested   "{-"  "-}")

-- _lex :: Parser a -> Parser a
-- _lex = lexeme pSpace 

pLocation :: Parser Location
pLocation = 
  char '@'
  *>  (char '_' $> Spine )
  <|> (char 'i' $> Input )
  <|> (char 'o' $> Output)
  <|> (char '^' $> Return)

spaceDef :: LanguageDef st
spaceDef = emptyDef
  { T.commentStart = "{-"
  , T.commentEnd = "-}"
  , T.commentLine = "--"
  , T.nestedComments = True
  , T.caseSensitive = False
  , T.identStart = alphaNum
  , T.identLetter = alphaNum <|> oneOf "_'"
  , T.opStart = T.opLetter spaceDef
  , T.opLetter = oneOf "@_"
  }

-- | A lexer for Space.
sL :: T.TokenParser st
sL = T.makeTokenParser spaceDef

whiteSpace = T.whiteSpace sL
lexeme = T.lexeme sL
angles = T.angles sL
identifier = T.identifier sL
braces = T.braces sL
