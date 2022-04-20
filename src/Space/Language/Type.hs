module Space.Language.Type where

import Space.Language.Location

newtype TVariableAtom = TVariableAtom String
    deriving (Eq,Show)

newtype TConstantAtom = TConstantAtom String
    deriving (Eq,Show)

data SType =
    TVariable TVariableAtom
    | TConstant TConstantAtom
    | TLocation Location SType
    | TVector SType SType
    | SType :=> SType
    | TEmpty
    deriving (Eq,Show)