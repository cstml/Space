module Space.Language.Type where

import Space.Language.Empty
import Space.Language.Location
import Space.Language.Vector
import Space.Language.Variable

import Prettyprinter

newtype TVariableAtom = TVariableAtom String
  deriving stock (Eq,Show,Ord)

instance Pretty TVariableAtom where
  pretty (TVariableAtom s) = pretty s

newtype TConstantAtom = TConstantAtom String
  deriving stock (Eq, Show, Ord)
  deriving Pretty via TVariableAtom

-- instance Pretty TConstantAtom where
--   pretty (TConstantAtom s) = pretty s

{- Same goes here for tagless initial.

See note in Term about tagless implementation.
-}

{-
data SType =
    TVariable TVariableAtom
    | TConstant TConstantAtom
    | TLocation Location SType
    | TVector SType SType
    | SType :=> SType
    | TEmpty
    deriving (Eq,Show)
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
          TLocation loc t con -> bracket $ "TLocation " <++> bracket (show loc)   <++> show t <++> show con
          TArrow t1 t2 con -> bracket $ "TArrow " <++> bracket (show t1) <++>  bracket (show t2) <++> show con
          TEmpty -> "TEmpty"

instance Pretty (SType a) where
  pretty = group .  go 
    where
      sep        = ","
      (<++>) x y = x <> pretty sep <+> y 
      bracket x = pretty "(" <> x <> pretty ")"
      curly x = "{" <> x <> "}"
      arrow x y = x <> pretty " -> " <+> y
      go :: SType a -> Doc ann
      go = \case 
          TVariable vAtom con -> pretty vAtom <++> pretty con
          TConstant cAtom con -> pretty cAtom <++> pretty con
          TLocation loc t con -> bracket (pretty t) <> pretty "@" <> pretty loc <++> pretty con
          TArrow t1 t2 con -> (arrow  (pretty t1) (pretty t2)) <++> pretty con
          TEmpty -> pretty "∅"
