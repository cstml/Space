module Space.Language.Type where

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

data SType l a
  = TVariable a (SType l a)
  | TConstant a (SType l a)
  | TLocation l (SType l a) (SType l a)
  | TArrow (SType l a) (SType l a) (SType l a)
  | TMany Integer (SType l a) (SType l a)
  | TEmpty
  deriving (Eq, Show, Ord, Generic, NFData, Functor)

infixl 7 ->:

-- | Utility Function for easy creation of arrows.
(->:) :: (SType a) -> (SType a) -> (SType a)
(->:) x y = TArrow x y TEmpty

instance (Pretty l, Pretty a) => Pretty (SType l a) where
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

instance Semigroup (SType a) where
  x <> y = case x of
    TEmpty -> y
    TVariable a c -> TVariable a $ c <> y
    TConstant a c -> TConstant a $ c <> y
    TLocation l t c -> TLocation l t $ c <> y
    TMany n t c -> TMany n t $ c <> y
    TArrow a b c -> TArrow a b $ c <> y

instance Monoid (SType a) where
  mempty = TEmpty

instance Unfoldable (SType a) where
  unfold = unfoldr go
   where
    go :: SType a -> Maybe (SType a, SType a)
    go = \case
      TVariable v con -> Just (TVariable v TEmpty, con)
      TConstant c con -> Just (TConstant c TEmpty, con)
      TLocation l t con -> Just (TLocation l t TEmpty, con)
      TArrow a b con -> Just (TArrow a b TEmpty, con)
      TMany n a con -> Just (TMany n a TEmpty, con)
      TEmpty -> Nothing

instance Foldable SType where
  foldMap f x = case x of
    TVariable a con -> f a <> foldMap f con
    TConstant a con -> f a <> foldMap f con
    TLocation l t con -> foldMap f t <> foldMap f con
  
-- FIXME
-- It should only sort by locations
instance Ord a => Normalise (SType a) where
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

depth :: (SType a) -> Integer
depth = \case
  TVariable _ con -> 1 + depth con
  TConstant _ con -> 1 + depth con
  TLocation l t con -> depth t + depth con
  TArrow a b con -> depth a + depth b + depth con
  TMany n a con -> n * depth a + depth con
  TEmpty -> 0
