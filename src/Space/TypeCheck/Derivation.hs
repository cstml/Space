module Space.TypeCheck.Derivation where

import Control.DeepSeq (NFData)
import Control.Lens
import GHC.Generics
import Space.Language

-- | A typing context is a set of varaible and type pairings.
newtype TContext = TContext {_tContext :: [(Variable, SType)]}
  deriving (Show, Eq, Generic, NFData)
  deriving newtype (Semigroup, Monoid)

makeLenses ''TContext

-- | A typing judgement attributes a type to each term in its context.
data TJudgement = TJudgement
  { _jContext :: TContext
  , _jTerm :: Term
  , _jType :: SType
  }
  deriving (Show, Eq, Generic, NFData)

makeLenses ''TJudgement

-- | A typing derivation is a logical proof, confirming the validity of a typing
-- judgement.
data TDerivation a
  = DEmpty !a
  | DSeq !(TDerivation a) !a
  | DPop !(TDerivation a) !a
  | DPush !(TDerivation a) !(TDerivation a) !a
  deriving (Show, Eq, Generic, NFData, Functor)

makeLenses ''TDerivation

-- | A pairing of types, that signifies their equivalence.
data TSubstitution = TSubstitution
  { _sInitialType :: SType
  , _sTargetType :: SType
  }
  deriving (Show, Eq, Ord, Generic, NFData)

makeLenses ''TSubstitution

applySubsContext :: TContext -> [TSubstitution] -> TContext
applySubsContext = \case
  TContext [] -> mempty
  TContext ((v, t) : xs) ->
    \subs -> TContext $ (v, foldr applyTypeSubs t subs) : (applySubsContext (TContext xs) subs ^. tContext)

applyTypeSubs :: TSubstitution -> SType -> SType
applyTypeSubs (TSubstitution tFrom tTo) tInit
  | tInit == tFrom = tTo
  | otherwise = tInit
