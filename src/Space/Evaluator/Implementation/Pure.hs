module Space.Evaluator.Implementation.Pure where

import Aux.Unfoldable

import Control.Arrow
import Control.Lens hiding (Empty, (:<), (<|))
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader

import Data.Kind
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Sequence
import Data.String
import Prettyprinter (pretty)
import Space.Aux.Evaluate
import Space.Evaluator.Exception
import Space.Evaluator.Machine
import Space.Evaluator.Memory
import Space.Evaluator.Stack
import Space.Language
import Space.Parser
import Space.Evaluator.Implementation

instance
  EvaluationMachine
    (ReaderT Environment (ExceptT MException (State (Memory Location Variable Term))))
    (Memory Location Variable Term)
    Variable
    Location
    Identity
  where
  getMemory = get

  putMemory = put

  updateMemory f = getMemory >>= putMemory . f

  putStack l s = getMemory >>= putMemory . (stacks . ix l .~ s)

  -- push1 :: Location -> Term -> SMachine ()
  push1 l t = do
    mem <- getMemory
    let b = case mem ^. stacks . at l of
          Just (Stack s) -> review stack $ t <| s
          Nothing -> review stack $ singleton t
    let nMem = mem & stacks . at l ?~ b
    putMemory nMem

  -- pop1 ::  Location -> SMachine Term
  pop1 l = do
    m <- getMemory
    case viewl $ m ^. stacks . ix l . stack of
      x :< xs -> do
        case xs of
          -- if it was the last element in the location we clear the location
          -- alltogether
          Empty -> do
            putMemory (m & stacks . at l .~ Nothing) >> pure x
          -- otherwsie we act normally
          _ -> do
            putStack l (review stack xs) >> pure x
      EmptyL -> do
        putMemory (m & stacks . at l .~ Nothing)
        pure SEmpty

  run state = flip runReaderT (Environment ()) >>> runExceptT >>> flip runState state >>> pure

  -- bind1 :: Variable -> Term -> SMachine ()
  bind1 v t = do
    m <- getMemory
    putMemory $ m & binds . at v ?~ t

  pop1Bind v l = do
    t <- pop1 l
    bind1 v t
    return t

  input = pure (fromString "")

  output = const $ pure ()

  getBind v = do
    mem <- getMemory
    case mem ^. binds . at v of
      Just t -> pure t
      Nothing -> pure $ SVariable v SEmpty

instance Evaluate MachineMemory Term MException Identity where
  eval mem = go . runIdentity . run mem . void . evaluate
   where
    go = \case
      (Left e, _) -> Identity $ Left e
      (Right (), mem) -> Identity $ Right mem

-- | Evaluate a Pure machine, and unwrap from the Identity.  Utility function
-- used in testing.
evaluate' :: Term -> Either MException MachineMemory
evaluate' = go . runIdentity . run mempty . void . evaluate
 where
  go = \case
    (Left e, _) -> Left e
    (Right (), mem) -> Right mem
