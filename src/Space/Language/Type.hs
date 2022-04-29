module Space.Language.Type where

import GHC.Generics
import Prettyprinter
import Space.Language.Empty
import Space.Language.Location
import Space.Language.Variable
import Space.Language.Vector

newtype TVariableAtom = TVariableAtom String
  deriving stock (Eq, Show, Ord)

instance Pretty TVariableAtom where
  pretty (TVariableAtom s) = pretty s

data TConstantAtom = TChar | TInt
  deriving stock (Eq, Show, Ord)
  deriving (Generic)

instance Pretty TConstantAtom where
  pretty = pretty . show

data SType
  = TVariable TVariableAtom SType
  | TConstant TConstantAtom SType
  | TLocation Location SType SType
  | TArrow SType SType SType
  | TEmpty
  deriving (Eq, Show, Ord)

instance Pretty SType where
  pretty = \case
    TVariable v con -> pretty v <+> pretty con
    TConstant a con -> pretty a <+> pretty con
    TLocation l ty con -> pretty l <+> parens (pretty ty) <+> pretty con
    TArrow ti to con -> braces (pretty ti <+> pretty "->" <+> pretty to) <+> pretty con
    TEmpty -> pretty "∅"
    
{-
{-

For now took the decision to not go down the path of correct by construction
(~GADTs) to allow easily interacting and "wrangling" with the types. I don't
exclude reimplementing the other way at a later time, but I first want a POC of
the checker.

-}
data SType a where
  TVariable :: TVariableAtom -> SType a -> SType TVariableAtom
  TConstant :: TConstantAtom -> SType a -> SType TConstantAtom
  TLocation :: Location -> SType a -> SType b -> SType Location
  TArrow :: SType a -> SType b -> SType c -> SType (a -> b)
  TEmpty :: SType Void

instance Show (SType a) where
  show =
    let bracket x = "(" <> x <> ")"
        sep = " "
        (<++>) x y = x <> sep <> y
     in \case
          TVariable vAtom con -> bracket $ "TVariable " <++> bracket (show vAtom) <++> show con
          TConstant cAtom con -> bracket $ "TConstant " <++> bracket (show cAtom) <++> show con
          TLocation loc t con -> bracket $ "TLocation " <++> bracket (show loc) <++> show t <++> show con
          TArrow t1 t2 con -> bracket $ "TArrow " <++> bracket (show t1) <++> bracket (show t2) <++> show con
          TEmpty -> "TEmpty"

instance Pretty (SType a) where
  pretty = group . go
   where
    sep = ","
    (<++>) x y = x <> pretty sep <+> y
    bracket x = pretty "(" <> x <> pretty ")"
    curly x = "{" <> x <> "}"
    arrow x y = x <> pretty " -> " <+> y
    go :: SType a -> Doc ann
    go = \case
      TVariable vAtom con -> pretty vAtom <++> pretty con
      TConstant cAtom con -> pretty cAtom <++> pretty con
      TLocation loc t con -> bracket (pretty t) <> pretty "@" <> pretty loc <++> pretty con
      TArrow t1 t2 con -> arrow (pretty t1) (pretty t2) <++> pretty con
      TEmpty -> pretty "∅"
-}
