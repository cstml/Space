module Space.Language.Variable where

import Control.DeepSeq (NFData)
import Control.Monad.Identity
import Data.String
import GHC.Generics
import Prettyprinter

newtype Variable = Variable String
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

instance Pretty Variable where
  pretty (Variable s) = pretty s

instance IsString Variable where
  fromString = Variable
