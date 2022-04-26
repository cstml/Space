module Space.Language.Location where

import Data.Kind
import GHC.TypeLits
import Prettyprinter
import Space.Aux.Show
import Space.Language.Variable

newtype Location = Location String
  deriving stock (Show)
  deriving newtype (Eq, Ord)
  deriving (Pretty) via Variable

{-
data Lo where
  In :: Lo
  Out :: Lo
  C :: Symbol -> Lo

instance Eq Lo where
  (==) = \case
    In -> \case In -> True
                _ -> False
    Out -> \case Out -> True
                 _ -> False
    C s -> \case C s' -> True
                 _ -> False

instance Show Lo where
  show = \case
    In -> "In"
    Out -> "Out"
    C s -> "C "

{- This is an attempt at making Location a Tagless Initial as well. It might backfire badly. -}
data Location (a :: Lo) b where
  LIn :: b -> Location In b
  LOut :: b -> Location Out b
  LC :: b -> Location (C "custom") b

instance Show b => Show (Location a b) where
  show = \case
    LIn t -> bracket "LIn " <> show t
    LOut t -> bracket "LOut " <> show t
    LC t -> bracket "LC " <> show t

instance (Eq b) => Eq (Location a b) where
  (==) = \case
    LIn b -> \case
      LIn b' -> b == b'
    _ -> const False

{-
instance PShow b => PShow (Location a b) where
  pShow = \case
    LIn t -> bracket "LIn " <> show t
    LOut t -> bracket "LOut " <> show t
    LC s t -> ""
-}
-}
