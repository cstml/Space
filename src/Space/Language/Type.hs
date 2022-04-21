module Space.Language.Type where

import Space.Aux.PShow
import Space.Language.Location
import Data.Void

newtype TVariableAtom = TVariableAtom String
    deriving (Eq,Show)

instance PShow TVariableAtom where
  pShow (TVariableAtom s) = s 

newtype TConstantAtom = TConstantAtom String
    deriving (Eq,Show)

instance PShow  TConstantAtom where
  pShow ( TConstantAtom s) = s 


{- Same goes here for tagless initial.

See note in Term about tagless implementation.
-}

{-
data SType =
    TVariable TVariableAtom
    | TConstant TConstantAtom
    | TLocation Location SType
    | TVector SType SType
    | SType :=> SType
    | TEmpty
    deriving (Eq,Show)
-}

data Vector a b where
  (:+:) :: a ->  b -> Vector a b
  LiftV :: a -> Vector a Void
  EmptyV :: Vector Void Void

data SType a where
  TVariable :: TVariableAtom -> SType a -> SType TVariableAtom
  TConstant :: TConstantAtom -> SType a -> SType TConstantAtom 
  TLocation :: Location -> SType b -> SType Location
  (:=>)     :: SType a -> SType b -> SType (a -> b)
  TEmpty    :: SType Void  

instance Show (SType a) where
  show =
    let
      bracket x = "(" <> x <> ")"
      sep = " "
    in \case
      TVariable vAtom con -> bracket $ "TVariable " <> show vAtom <> show con
      TConstant cAtom con -> bracket $ "TConstant " <> show cAtom <> show con 
      TLocation loc con -> bracket $ show loc <> show con
      (:=>) t1 t2 -> bracket $ show t1 <> " :=> " <> show t2
      TEmpty -> "TEmpty"

instance PShow (SType a) where
  pShow =
    let
      bracket x = "(" <> x <> ")"
      curly x = "{" <> x <> "}"
      arrow x y = x <> " -> " <> y
      sep = " "
    in \case
      TVariable vAtom con -> pShow vAtom <> show con
      TConstant cAtom con -> show cAtom <> show con 
      TLocation loc con -> show loc <> show con
      (:=>) t1 t2 -> arrow (pShow t1) (pShow t2)
      TEmpty -> "∅"
    
