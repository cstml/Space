module Space.Language.Term where

import Space.Language.Variable
import Space.Language.Location
import Space.Language.Type

data Term =
    SVariable Variable
    | SInteger Int
    | SChar Char
    | SPush Term Location
    | SPop Variable Location
    | SEmpty
    deriving (Eq,Show)