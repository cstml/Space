{-# LANGUAGE FunctionalDependencies #-}

module Space.TypeCheck.Derive where

import Aux.Unfoldable
import Control.Lens
import Control.Monad.Freer
import Control.Monad.Freer.TH
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Identity
import Data.Bifunctor
import Data.Kind
import Data.List
import Data.String
import GHC.Generics
import Prettyprinter (pretty)
import Space.Language
import Space.Parser
import Space.TypeCheck.Derivation
import Space.TypeCheck.Exception
import Space.TypeCheck.Properties

type VarStream = [String]

data TDerivationInfo = TDerivationInfo
  { _numVarStream :: [String]
  , _alphaVarStream :: [Char]
  }
  deriving (Show, Eq)

makeLenses ''TDerivationInfo

-- | Build the derivation using the terms
derive1 :: Term -> TDerivation Term
derive1 = f
 where
  f :: Term -> TDerivation Term
  f term = case term of
    SEmpty -> DEmpty term
    SInteger i SEmpty -> DVar term
    SInteger i cont -> DSeq term (DVar $ SInteger i SEmpty) (derive1 cont)
    SVariable v cont -> DSeq term (DVar $ SVariable v SEmpty) (derive1 cont)
    SChar c cont -> DSeq term (DVar $ SChar c SEmpty) (derive1 cont)
    SPush t l cont -> DPush term (derive1 t) (derive1 cont)
    SPop v l cont -> DPop term (DVar $ SVariable v SEmpty) (derive1 cont)

testDerive1 = pretty . fmap pretty . derive1 . parseTermTest

-- | Give types to trivial terms, give the most generic type to everything else.
derive1t2 :: TDerivation Term -> TDerivation TJudgement
derive1t2 = fst . flip runState stream . sequence . fmap f
 where
  f :: Term -> State [String] TJudgement
  f x = case x of
    SEmpty ->
      let typ = TEmpty ->: TEmpty in pure $ TJudgement (TContext [(x, typ)]) x typ
    SInteger _ SEmpty ->
      let typ = TEmpty ->: TConstant TInt TEmpty in pure $ TJudgement (TContext [(x, typ)]) x typ
    SChar _ SEmpty ->
      let typ = TEmpty ->: TConstant TChar TEmpty in pure $ TJudgement (TContext [(x, typ)]) x typ
    SVariable _ SEmpty -> do
      typ <- getFreshT
      pure $ TJudgement (TContext [(x, typ)]) x typ
    _ -> do
      typ <- getFreshT
      pure $ TJudgement (TContext [(x, typ)]) x typ

  -- Stream of fresh variables.
  stream = [x : y : [] | y <- ['1' ..], x <- ['a' .. 'z']]

  -- Get a nice fresh type.
  getFreshT :: State [String] SType
  getFreshT = do
    freshV <- getFreshVar
    pure $ TVariable (TVariableAtom freshV) TEmpty
   where
    getFreshVar :: State [String] String
    getFreshVar = do
      x' <- get
      case x' of
        (x : xs) -> do
          put xs
          return x
        _ -> error "This should be an infinite stream - should not happen."

-- | Utility functio to test
testDerive2 = pretty . fmap pretty . derive1t2 . derive1 . parseTermTest

{-
--type TContext = [(TVariableAtom, SType)]

class GetFresh (m :: Type -> Type) (a :: Type) where
  fresh :: m a

class HasContext (m :: Type -> Type) (a :: Type) (b :: Type)  | m a -> b where
  whats :: a -> m (Maybe b)
  add   :: a -> m ()

class CanFail (m :: Type -> Type) (e :: Type) where
   fail :: e -> m a

infer :: ( Monad m
         , GetFresh m TVariableAtom
         , HasContext m Term SType
         , CanFail m TCError
         )
         => Term -> m SType
infer = f
  where
    f = \case
      SVariable v cont -> do
        let termStep = SVariable v SEmpty
        ct <- whats termStep
        case ct of
          Nothing -> do
            ft <- fresh
            add (termStep,ft)
            (ft <£>) <$> (infer cont)
          Just t -> (t <£>) <$> (infer cont)

{-
SInteger _ cont -> TConstant TInt  <$> infer cont
      SChar _ cont  -> TConstant TChar <$> infer cont
      SPush t l cont -> TLocation l <$> infer t <*> infer cont
    --  SPop Variable Location Term
    --  SPopT Variable Location SType Term
    --  SEmpty
-}
(<£>) :: ( Monad m
         , GetFresh m TVariableAtom
         , HasContext m Term SType
         , CanFail m TCError
         )
         => SType ->  SType -> m SType
(<£>) = undefined
{-
    TEmpty -> y
    TVariable a c -> TVariable a $ c <> y
    TConstant a c -> TConstant a $ c <> y
    TLocation l t c -> TLocation l t $ c <> y
    TMany n t c -> TMany n t $ c <> y
    TArrow a b c -> TArrow a b $ c <> y
-}
{-
newtype Consume a = Consume { unConsume :: Except TCError a }
  deriving stock (Eq, Ord, Show)
  deriving newtype (Monad, Functor, Applicative)

data Union a = Union
  { subs :: Substitution a
  , remainingLeft :: SType
  , remainingRight :: SType
  }

class Consumable (a :: Type) where
  (<£>) :: a -> a -> Consume [Union a]

class FirstElement (a :: Type) where
  gFirst :: a -> a

instance Consumable SType where
  term1 <£> term2 =
    case gFirst term1 of
          TEmpty -> undefined
          TVariable a c -> undefined
          TConstant a c -> undefined
          TLocation l t c -> undefined
          TArrow a b c -> undefined

{-
    case unfold term1 of
     t1:t1s ->
       case t1 of
         TVariable _ _ ->
           case unfold term2 of
             t2:t2s -> ((Substitution t1 t2) :) <$> (mconcat t1s <£> mconcat t2s)
             _ -> pure [Substitution t1 TEmpty]
         _ -> case unfold term2 of
           t2:t2s ->  Consume $ throwE TCError
-}
-}

-}
