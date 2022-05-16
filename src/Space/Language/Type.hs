module Space.Language.Type where

import Space.TypeCheck.Properties
import Aux.Unfoldable (Unfoldable (..))
import Control.DeepSeq (NFData)
import Data.List (unfoldr)
import Data.String (IsString (..))
import GHC.Generics (Generic)
import Prettyprinter (
  Pretty (pretty),
  braces,
  brackets,
  parens,
  (<+>),
 )
import Space.Language.Empty ()
import Space.Language.Location (Location)
import Space.Language.Variable ()
import Space.Language.Vector ()
import Space.TypeCheck.Properties (Normalise (..))
import Data.Kind

newtype TVariableAtom = TVariableAtom String
  deriving stock (Eq, Show, Ord)
  deriving (Generic, NFData)

instance Pretty TVariableAtom where
  pretty (TVariableAtom s) = pretty s

instance IsString TVariableAtom where
  fromString = TVariableAtom

data TConstantAtom = TChar | TInt
  deriving stock (Eq, Ord, Show)
  deriving (Generic, NFData)

instance Pretty TConstantAtom where
  pretty =
    let go = \case
          TChar -> "Ch"
          TInt -> "Z"
     in pretty @String . go

data Rewrite
  = AsIs
  | NormalForm

data SType 
  = TVariable TVariableAtom SType
  | TConstant TConstantAtom SType
  | TLocation Location SType SType
  | TArrow    SType SType SType
  | TMany     Integer SType SType 
  | TEmpty
  deriving (Eq, Show, Ord, Generic, NFData)


instance Semigroup SType where
  x <> y = case x of
    TEmpty -> y
    TVariable a c -> TVariable a $ c <> y
    TConstant a c -> TConstant a $ c <> y
    TLocation l t c -> TLocation l t $ c <> y
    TMany n t c -> TMany n t $ c <> y
    TArrow a b c -> TArrow a b $ c <> y

instance Monoid SType where
  mempty = TEmpty

instance Reduce SType where
  reduce1 = \case
    TEmpty -> pure TEmpty
    TVariable a con -> case con of
      TVariable _ con' -> TVariable a <$> (reduce1 con')


infixl 7 ->:

-- | Utility Function for easy creation of arrows.
(->:) :: SType  -> SType -> SType
(->:) x y = TArrow x y TEmpty

instance  Pretty SType where
  pretty =
    let sep = pretty @String ";"
        (<++>) x y = x <> sep <> y
        multip = pretty @String "."
     in \case
          TVariable v con -> case con of
            TEmpty -> pretty v
            _ -> pretty v <+> pretty con
          TConstant a con -> case con of
            TEmpty -> pretty a
            _ -> pretty a <++> pretty con
          TLocation l ty con ->
            case con of
              TEmpty -> parens (pretty ty) <> pretty l
              _ -> parens (pretty ty) <> pretty l <++> pretty con
          TArrow ti to con -> case con of
            TEmpty -> brackets (pretty ti <+> pretty @String "->" <+> pretty to)
            _ -> brackets (pretty ti <+> pretty @String "->" <+> pretty to) <++> pretty con
          TMany n ti con -> case con of
            TEmpty -> pretty n <> multip <> braces (pretty ti)
            _ -> pretty n <> multip <> braces (pretty ti) <++> pretty con
          TEmpty -> pretty @String "{}"

instance Unfoldable SType where
  unfold = unfoldr go
   where
    go :: SType -> Maybe (SType, SType)
    go = \case
      TVariable v con -> Just (TVariable v TEmpty, con)
      TConstant c con -> Just (TConstant c TEmpty, con)
      TLocation l t con -> Just (TLocation l t TEmpty, con)
      TArrow a b con -> Just (TArrow a b TEmpty, con)
      TMany n a con -> Just (TMany n a TEmpty, con)
      TEmpty -> Nothing
  
-- FIXME
-- It should only sort by locations
instance Normalise SType where
  normalise x = case x of
    TEmpty -> x
    TVariable a c -> TVariable a (normalise c)
    TConstant a c -> TConstant a (normalise c)
    TLocation l t c ->
      let uc = unfold c
          t' = normalise t
          z = TLocation l t' TEmpty
       in case uc of
            (c' : cs') ->
              if z < c'
                then normalise . mconcat $ [c', z, mconcat cs']
                else z <> normalise (c' <> mconcat cs')
            [] -> TLocation l t' (normalise c)
    TArrow a b c -> TArrow (normalise a) (normalise b) (normalise c)
    TMany n t c -> TMany n (normalise t) (normalise c)

depth :: SType -> Integer
depth = \case
  TVariable _ con -> 1 + depth con
  TConstant _ con -> 1 + depth con
  TLocation l t con -> depth t + depth con
  TArrow a b con -> depth a + depth b + depth con
  TMany n a con -> n * depth a + depth con
  TEmpty -> 0

