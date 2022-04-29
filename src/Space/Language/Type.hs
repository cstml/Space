module Space.Language.Type where

import Data.String
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

instance IsString TVariableAtom where
  fromString = TVariableAtom

data TConstantAtom = TChar | TInt
  deriving stock (Eq, Ord,Show)
  deriving (Generic)

instance Pretty TConstantAtom where
  pretty =
    let go = \case
          TChar -> "Ch"
          TInt  -> "Z"
    in pretty . go  

data SType
  = TVariable TVariableAtom SType
  | TConstant TConstantAtom SType
  | TLocation Location SType SType
  | TArrow SType SType SType
  | TMany Integer SType SType
  | TEmpty
  deriving (Eq, Show, Ord)

infixl 7 ->:

-- | Utility Function for easy creation of arrows. 
(->:) :: SType -> SType -> SType
(->:) x y = TArrow x y TEmpty

instance Pretty SType where
  pretty =
    let
      sep = pretty ";"
      (<++>) x y =  x <> sep <> y
      multip = pretty "."
    in
        \case
          TVariable v con -> case con of
            TEmpty -> pretty v
            _ -> pretty v <+> pretty con
          TConstant a con -> case con of
            TEmpty -> pretty a 
            _ -> pretty a <++> pretty con
 
          TLocation l ty con ->
            let
              prettyl = case l of
--                DLocation -> pretty ""
                _ -> pretty l
            in case con of
              TEmpty -> parens (pretty ty) <> prettyl
              _ ->  parens (pretty ty) <> prettyl <++> pretty con
              
          TArrow ti to con -> case con of
            TEmpty -> brackets (pretty ti <+> pretty "->" <+> pretty to) 
            _ -> brackets (pretty ti <+> pretty "->" <+> pretty to) <++> pretty con
            
          TMany n ti con ->   case con of
            TEmpty -> pretty n <> multip <> braces (pretty ti)
            _ -> pretty n <> multip <> braces (pretty ti) <++> pretty con
          TEmpty -> pretty "{}"
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
