{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Space.Evaluator.Machine where

import Control.Exception
import Control.Lens hiding (Empty, (:<), (<|))
import Control.Monad.Freer
import Control.Monad.Freer.TH
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
import Space.Evaluator.Exception
import Space.Evaluator.Memory
import Space.Evaluator.Stack
import Space.Language

type MachineMemory = Memory Location Variable Term

type MachineStack = Stack Term

data EvalMachine (return :: Type) where
  GetMemoryM :: EvalMachine MachineMemory
  PutMemoryM :: MachineMemory -> EvalMachine ()
  UpdateMemoryM :: (MachineMemory -> MachineMemory) -> EvalMachine ()
  OutputM :: Show a => a -> EvalMachine ()
  InputM :: IsString a => EvalMachine a
  Pop1M :: Location -> EvalMachine Term
  Push1M :: Location -> Term -> EvalMachine ()
  Bind1M :: Variable -> Term -> EvalMachine ()
  GetBindM :: Variable -> EvalMachine Term
  PopGammaM :: EvalMachine Term
  PushGammaM :: Term -> EvalMachine ()
  ErrorM :: forall a. String -> EvalMachine a

makeEffect ''EvalMachine

data StateMachine (return :: Type) where
  StepM :: Term -> StateMachine MachineMemory

makeEffect ''StateMachine
