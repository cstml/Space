module Space.TypeCheck.TCTerm where

data CTerm =
  CVariable String
  | CPop  Location CTerm
  | CPush Location CTerm
  deriving (Eq,Show)
