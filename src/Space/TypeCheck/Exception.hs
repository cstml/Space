module Space.TypeCheck.Exception where

data TCError
  = TCError
  | TCErrorString String
  deriving (Show,Eq,Ord)
