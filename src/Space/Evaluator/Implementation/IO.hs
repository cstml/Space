module Space.Evaluator.Implementation.IO where

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
import Space.Evaluator.Implementation.Pure hiding (eval)
import Space.Evaluator.Machine
import Space.Evaluator.Memory
import Space.Evaluator.Stack
import Space.Language
import Space.Parser
import Space.Evaluator.Implementation 

instance
  EvaluationMachine
    (ReaderT Environment (ExceptT MException (StateT MachineMemory IO)))
    MachineMemory
    Variable
    Location
    IO
  where
  output = liftIO . print
  
  getMemory = get
  
  putMemory = put
  
  updateMemory f = getMemory >>= putMemory . f
  
  putStack l s = getMemory >>= putMemory . (stacks . ix l .~ s)

  push1 l t = do
    mem <- getMemory
    let b = case mem ^. stacks . at l of
          Just (Stack s) -> review stack $ t <| s
          Nothing -> review stack $ singleton t
    let nMem = mem & stacks . at l ?~ b
    putMemory nMem

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

  bind1 v t = do
    m <- getMemory
    putMemory $ m & binds . at v ?~ t

  pop1Bind v l = do
    t <- pop1 l
    bind1 v t
    return t

  getBind v = do
    mem <- getMemory
    case mem ^. binds . at v of
      Just t -> pure t
      Nothing -> pure $ SVariable v SEmpty

  input = fromString <$> liftIO getLine

  run state = flip runReaderT (Environment ()) >>> runExceptT >>> flip runStateT state

instance Evaluate MachineMemory Term MException IO where
  eval mem term = do
    res <- run mem . void . evaluate $ term
    return $ go res
   where
    go = \case
      (Left e, _) -> Left e
      (Right (), mem) -> Right mem
