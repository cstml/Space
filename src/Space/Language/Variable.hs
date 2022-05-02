module Space.Language.Variable where

import Control.Monad.Identity
import Data.String
import Prettyprinter

newtype Variable = Variable String
  deriving (Eq, Ord) via String

instance Show Variable where
  show (Variable s) = s

instance Pretty Variable where
  pretty (Variable s) = pretty s

instance IsString Variable where
  fromString = Variable
