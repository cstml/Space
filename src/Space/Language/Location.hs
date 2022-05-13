module Space.Language.Location (
  Location (..),
) where

import Control.DeepSeq
import Data.String
import GHC.Generics
import Prettyprinter
import Space.Aux.Show
import Space.Language.Variable

data Location = DLocation | Location String
  deriving stock (Show, Eq, Ord)
  deriving (Generic, NFData)

instance Pretty Location where
  pretty = \case
    DLocation -> pretty "@"
    Location l -> pretty "@" <> pretty l

instance IsString Location where
  fromString = Location
