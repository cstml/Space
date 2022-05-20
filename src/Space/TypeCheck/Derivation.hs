module Space.TypeCheck.Derivation where

import Aux.Unfoldable
import Control.DeepSeq (NFData)
import Control.Lens
import Data.Kind
import GHC.Generics
import Prettyprinter (pretty)
import Space.Language

-- | A typing context is a set of varaible and type pairings.
newtype TContext = TContext {_tContext :: [(Term, SType)]}
  deriving (Show, Eq, Generic)
  deriving newtype (Semigroup, Monoid)
  deriving anyclass (NFData)

makeLenses ''TContext

-- | A typing judgement attributes a type to each term in its context.
data TJudgement = TJudgement
  { _jContext :: TContext
  , _jTerm :: Term
  , _jType :: SType
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (NFData)

makeLenses ''TJudgement

-- | A typing derivation is a logical proof, confirming the validity of a typing
-- judgement.
data TDerivation a
  = DEmpty !a
  | DSeq !a !(TDerivation a)
  | DPop !a !(TDerivation a) !(TDerivation a)
  | DPush !a !(TDerivation a) !(TDerivation a)
  deriving (Show, Eq, Generic, NFData, Functor)

makeLenses ''TDerivation

-- | A pairing of types, that signifies their equivalence.
data TSubstitution = TSubstitution
  { _sInitialType :: SType
  , _sTargetType :: SType
  }
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)

makeLenses ''TSubstitution

-- | Apply substitutions to context
applySubsContext :: TContext -> [TSubstitution] -> TContext
applySubsContext = \case
  TContext [] -> mempty
  TContext ((v, t) : xs) ->
    \subs -> TContext $ (v, mconcat $ tSubsToFn subs <$> unfold t) : (applySubsContext (TContext xs) subs ^. tContext)

-- | Create a substitution function from a list of substitutions.
tSubsToFn :: [TSubstitution] -> SType -> SType
tSubsToFn ((TSubstitution tFrom tTo) : xs) = tSubsToFn xs . (\x -> if x == tFrom then tTo else x)

-- | Pretty Show instance that replaces the context with gamma.
pShow' :: TDerivation TJudgement -> String
pShow' d = unlines (reverse strs)
 where
  (_, _, _, strs) = showD d

  showT :: SType -> String
  showT = show . pretty

  showJ :: TJudgement -> String
  showJ (TJudgement cx n t) = mconcat ["Γ ", "|- ", (show . pretty) n, " : ", showT t]

  showL :: Int -> Int -> Int -> String
  showL l m r = mconcat [replicate l ' ', replicate m '-', replicate r ' ']

  showD :: TDerivation TJudgement -> (Int, Int, Int, [String])
  showD (DEmpty j) = (0, k, 0, [s, showL 0 k 0]) where s = showJ j; k = length s
  showD (DSeq j d') = addrule (showJ j) (showD d')
  showD (DPop j d' e) = addrule (showJ j) (sidebyside (showD d') (showD e))
  showD (DPush j d' e) = addrule (showJ j) (sidebyside (showD d') (showD e))
  --        showD (Fusion j d' e) = addrule (showJ j) (sidebyside (showD d') (showD e))
  addrule :: String -> (Int, Int, Int, [String]) -> (Int, Int, Int, [String])
  addrule x (l, m, r, xs)
    | k <= m =
      (ll, k, rr, (replicate ll ' ' ++ x ++ replicate rr ' ') : showL l m r : xs)
    | k <= l + m + r =
      (ll, k, rr, (replicate ll ' ' ++ x ++ replicate rr ' ') : showL ll k rr : xs)
    | otherwise =
      (0, k, 0, x : replicate k '-' : [replicate (-ll) ' ' ++ y ++ replicate (-rr) ' ' | y <- xs])
   where
    k = length x; i = div (m - k) 2; ll = l + i; rr = r + m - k - i
  extend :: Int -> [String] -> [String]
  extend i strs' = strs' ++ repeat (replicate i ' ')
  sidebyside :: (Int, Int, Int, [String]) -> (Int, Int, Int, [String]) -> (Int, Int, Int, [String])
  sidebyside (l1, m1, r1, d1) (l2, m2, r2, d2)
    | length d1 > length d2 =
      (l1, m1 + r1 + 2 + l2 + m2, r2, [x ++ "  " ++ y | (x, y) <- zip d1 (extend (l2 + m2 + r2) d2)])
    | otherwise =
      (l1, m1 + r1 + 2 + l2 + m2, r2, [x ++ " " ++ y | (x, y) <- zip (extend (l1 + m1 + r1) d1) d2])
