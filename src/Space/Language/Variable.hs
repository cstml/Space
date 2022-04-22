module Space.Language.Variable where

import Control.Monad.Identity
import Space.Aux.PShow

newtype Variable = Variable String
  deriving (Eq, Show, Ord) via String

instance PShow Variable where
  pShow (Variable s) = s
