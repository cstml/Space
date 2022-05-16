module Space.TypeCheck.Exception where

-- | Type Checking Error.
data TCError
  = TCError
  | TCErrorString String
  deriving (Show, Eq, Ord)
