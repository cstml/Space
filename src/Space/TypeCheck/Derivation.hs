module Space.TypeCheck.Derivation where

import Space.Language
import Control.Lens

-- | A typing judgement attributes a type to each term in its context.
data TJudgement =
  TJudgement
   { _jContext :: [(Variable, SType)]
   , _jTerm    :: Term
   , _jType    :: SType
   }
  deriving (Show,Eq)

-- | A typing derivation is a logical proof, confirming the validity of a typing
-- judgement.
data TDerivation 
    = DEmpty      !TJudgement
    | DSeq        !TJudgement !TDerivation
    | DPop        !TJudgement !TDerivation
    | DPush       !TJudgement !TDerivation !TDerivation  
  deriving (Show,Eq)

-- | A pairing of types, that signifies their equivalence.
data TSubstitution = TSubstitution
  { _sInitialType :: SType
  , _sTargetType :: SType
  }
  deriving (Show,Eq)

makeLenses ''TJudgement
makeLenses ''TDerivation
makeLenses ''TSubstitution
