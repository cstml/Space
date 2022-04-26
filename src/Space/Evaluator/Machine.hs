{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Space.Evaluator.Machine where

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
import Space.Evaluator.Exception
import Space.Evaluator.Memory
import Space.Evaluator.Stack
import Space.Language
import System.IO.Error.Lens (location)

type MachineMemory = Memory Location Variable Term

type MachineStack = Stack Term

newtype Environment = Environment ()

type SMachine a = ReaderT Environment (ExceptT MException (State MachineMemory)) a

class
  EvaluationMachine
    (mac :: Type -> Type)
    (mem :: Type)
    (var :: Type)
    (location :: Type)
    | mac -> mem
    , mac -> var
    , mac -> location
  where
  getMemory :: mac mem
  putMemory :: mem -> mac ()
  updateMemory :: (mem -> mem) -> mac ()
  putStack :: location -> MachineStack -> mac ()
  pop1 :: location -> mac Term
  pop1Bind :: var -> location -> mac Term
  bind1 :: var -> Term -> mac ()
  push1 :: location -> Term -> mac ()
  run :: mac () -> mem

instance EvaluationMachine (ReaderT Environment (ExceptT MException (State MachineMemory))) MachineMemory Variable Location where
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

  run = flip runReaderT (Environment ()) >>> runExceptT >>> flip execState mempty

  -- bind1 :: Variable -> Term -> SMachine ()
  bind1 v t = do
    m <- getMemory
    putMemory $ m & binds . at v ?~ t

  -- pop1Bind :: Variable -> Location -> SMachine Term
  pop1Bind v l = do
    t <- pop1 l
    bind1 v t
    return t

toNum :: Term -> Maybe Int
toNum = \case
  SInteger x SEmpty -> pure x
  _ -> Nothing

fromNum :: Int -> Term
fromNum = flip SInteger SEmpty

evaluate ::
  (EvaluationMachine (ReaderT Environment (ExceptT MException (State MachineMemory))) MachineMemory v Location) =>
  Term ->
  ReaderT Environment (ExceptT MException (State MachineMemory)) MachineMemory
evaluate = \case
  SEmpty -> getMemory
  (unfoldTerm -> x : cons) ->
    let con = mconcat cons
     in case x of
          SInteger _ _ -> push1 (Location "Ho") x >> evaluate con
          SChar _ _ -> push1 (Location "Ho") x >> evaluate con
          SPop v l _ -> (bind1 v =<< pop1 l) >> evaluate con
          SVariable (Variable v) _ ->
            let op o = do
                  a <- toNum <$> pop1 (Location "Ho")
                  b <- toNum <$> pop1 (Location "Ho")
                  let res = o <$> a <*> b
                  case res of
                    Nothing -> lift . throwE $ WrongType ""
                    Just res' -> push1 (Location "Ho") (fromNum res') >> evaluate con
             in case v of
                  "+" -> op (+)
                  "*" -> op (*)
                  "^" -> op (^)
                  "/" -> op div
                  _ -> do
                    mem <- getMemory
                    let t = mem ^. binds . ix (Variable v)
                    evaluate $ t <> con

--  _ -> fail "Not Complteted "

(>>>) = flip (.)

evaluate' :: Term -> MachineMemory
evaluate' = run . void . evaluate
