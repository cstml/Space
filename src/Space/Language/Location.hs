module Space.Language.Location where

import Data.String
import Prettyprinter
import Space.Aux.Show
import Space.Language.Variable

data Location = DLocation | Location String
  deriving stock (Show, Eq, Ord)

instance Pretty Location where
  pretty = \case
    DLocation -> pretty "@"
    Location l -> pretty "@" <> pretty l

instance IsString Location where
  fromString = Location
