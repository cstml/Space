{-# Language GADTs #-}
module Space.Language.Term where

import Space.Language.Variable
import Space.Language.Location
import Space.Language.Type
import Space.Aux.PShow

import Data.Void

{- This sort of AST is the tag specifc one used in my previous project. Will
 attempt at a tagless initial together with a tagless final implementation.

More on the two techniques can be found here:

- https://serokell.io/blog/introduction-tagless-final.


data Term =
    SVariable Variable
    | SInteger Int
    | SChar Char
    | SPush Term Location
    | SPop Variable Location
    | SEmpty
    deriving (Eq,Show)
-}

data Term a where
  SVar      :: Variable -> Term a -> Term Variable
  SInt      :: Int -> Term a -> Term Int
  SChar     :: Char -> Term a -> Term Char
  SPush     :: Term a -> Location -> Term c -> Term b
  SPop      :: Term Variable -> Location -> Term a -> Term b
  SPopTyped :: Term Variable -> Location -> SType t ->Term a -> Term b
  SEmpty    :: Term Void

instance Show (Term a) where
  show = 
    let
      sep = " "
      bracket x = "(" <> x <> ")"
    in
      \case
        SVar var con -> bracket $ "SVar " <> show var <> sep <> show con
        SInt int con -> bracket $ "SInt " <> show int <> sep <> show con
        SChar ch con -> bracket $ "SChar " <> show ch <> sep <> show con
        SPush pusht loc con -> bracket $ "SPush " <> bracket (show pusht) <> show loc <> show con         
        SPop var loc con -> bracket $ "SPop " <> show var <> show loc <> show con
        SEmpty -> "SEmpty"

instance PShow (Term a) where
  pShow = 
    let
      sep = " "
      cur x = "{" <> x <> "}"
      ang x = "<" <> x <> ">"
      squ x = "[" <> x <> "]"
    in
      \case
        SVar var con -> pShow var <> sep <> pShow con
        SInt int con -> show int <> sep <> pShow con
        SChar ch con -> show ch <> sep <> pShow con
        SPop var loc con ->  ang (pShow var) <> pShow loc <> pShow con
        SPush pusht loc con -> squ (pShow pusht) <> pShow loc <> pShow con         
        SEmpty -> "*"
