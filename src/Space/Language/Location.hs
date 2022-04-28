module Space.Language.Location where

import Data.Kind
import GHC.TypeLits
import Prettyprinter
import Space.Aux.Show
import Space.Language.Variable

newtype Location = Location String
  deriving stock (Show)
  deriving newtype (Eq, Ord)

instance Pretty Location where
  pretty (Location l) = pretty "@" <> pretty l 
