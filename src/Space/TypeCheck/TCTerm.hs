module Space.TypeCheck.TCTerm where

import Space.Language

data CTerm =
  CVariable String
  | CPop  Location CTerm
  | CPush Location CTerm
  deriving (Eq,Show)
