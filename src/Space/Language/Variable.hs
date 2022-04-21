module Space.Language.Variable where

import Space.Aux.PShow
import Control.Monad.Identity 

newtype Variable = Variable String
  deriving (Eq, Show, Ord) via String

instance PShow Variable where
  pShow (Variable s) = s
