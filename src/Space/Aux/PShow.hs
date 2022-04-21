module Space.Aux.PShow where

import Data.Kind

class PShow (a :: Type) where
  pShow :: a -> String
