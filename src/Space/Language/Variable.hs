module Space.Language.Variable where

import Space.Aux.PShow

newtype Variable = Variable String
    deriving (Eq,Show, Ord)

instance PShow Variable where
  pShow (Variable s) = s
