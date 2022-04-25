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
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Sequence

import Space.Evaluator.Exception
import Space.Evaluator.Memory
import Space.Evaluator.Stack
import Space.Language
import System.IO.Error.Lens (location)

type MachineStack = Stack Term

newtype Environment = Environment ()

type SMachine a = ReaderT Environment ({-ExceptT MException-} (State MachineMemory)) a

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
    let a :: Maybe MachineStack = mem ^. stacks . at l
    let b = case a of
          Just (Stack s) -> review stack $ t <| s
          Nothing -> review stack $ singleton t
    let nMem = mem & stacks . at l ?~ b
    --Just s ->  mem & (stacks . at l) ?~ (Just . Stack $ t <| s)
    --Nothing -> mem
    putMemory nMem

  -- pop1 ::  Location -> SMachine Term
  pop1 l = do
    m <- getMemory
    case viewl $ view (stacks . ix l . stack) m of
      x :< xs -> putStack l (review stack xs) >> pure x
      EmptyL -> pure SEmpty

  run = flip runReaderT  (Environment ()) >>> runExceptT >>>flip execState mempty 

  -- bind1 :: Variable -> Term -> SMachine ()
  bind1 v t = do
    m <- getMemory
    putMemory $ m & binds . ix v .~ t

  -- pop1Bind :: Variable -> Location -> SMachine Term
  pop1Bind v l = do
    t <- pop1 l
    bind1 v t
    return t

type Terms = [Term]

toNum :: Term -> Int
toNum = \case
  SInteger x -> x
  _ -> error "Not A number"

fromNum = SInteger

evaluate ::
  (EvaluationMachine (ReaderT Environment (ExceptT MException (State MachineMemory))) MachineMemory v Location) =>
  Terms -> ReaderT Environment (ExceptT MException (State MachineMemory)) MachineMemory
evaluate = \case
  [] -> getMemory
  (x : xs) -> case x of
    (SInteger _) -> push1 (Location "Ho") x >> evaluate xs
    (SVariable (Variable v)) ->
      let op o = do
            a <- toNum <$> pop1 (Location "Ho")
            b <- toNum <$> pop1 (Location "Ho")
            push1 (Location "Ho") (fromNum $ a `o` b)
            evaluate xs
       in case v of
            "+" -> op (+)
            "*" -> op (*)
            "^" -> op (^)
            "/" -> op div
--  _ -> fail "Not Complteted "

(>>>) = flip (.)


evaluate' :: Terms ->  {-Either MException-} MachineMemory
evaluate' =  run . void . evaluate
