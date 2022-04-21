module Space.Language.Location where

import Data.Kind
import GHC.TypeLits
import Space.Aux.PShow
import Space.Aux.Show

data Lo where
  In :: Lo
  Out :: Lo
  C :: Symbol -> Lo

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

{-
instance PShow b => PShow (Location a b) where
  pShow = \case
    LIn t -> bracket "LIn " <> show t
    LOut t -> bracket "LOut " <> show t
    LC s t -> ""
-}
