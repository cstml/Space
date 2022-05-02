module Space.Language.Variable where

import Data.String
import Prettyprinter

newtype Variable = Variable String
  deriving (Eq, Ord, Show)

instance Pretty Variable where
  pretty (Variable s) = pretty s

instance IsString Variable where
  fromString = Variable
