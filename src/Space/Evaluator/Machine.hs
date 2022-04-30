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

import Data.String
import Data.Kind
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Sequence
import Space.Evaluator.Exception
import Space.Evaluator.Memory
import Space.Evaluator.Stack
import Space.Language

type MachineStack = Stack Term

class  EvaluationMachine
    (mac :: Type -> Type)
    (mem :: Type)
    (var :: Type)
    (location :: Type)
    (m2 :: Type -> Type)
    | mac -> mem
    , mac -> var
    , mac -> location
    , mac -> m2 
  where
  getMemory :: mac mem
  putMemory :: mem -> mac ()
  updateMemory :: (mem -> mem) -> mac ()
  run :: mem -> mac () -> m2 (Either MException (), mem)
  putStack :: location -> MachineStack -> mac ()
  pop1 :: location -> mac Term
  pop1Bind :: var -> location -> mac Term
  bind1 :: var -> Term -> mac ()
  push1 :: location -> Term -> mac ()

  input ::  (IsString a) => mac a
  output :: (IsString a) => a -> mac ()
