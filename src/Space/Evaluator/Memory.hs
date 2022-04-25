{-# LANGUAGE TemplateHaskell #-}

module Space.Evaluator.Memory where

import Control.Lens hiding ((:<), (<|))
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Sequence
import Space.Evaluator.Stack
import Space.Language
import Space.Language.Empty

data Memory location binder term = Memory
  { _stacks :: Map location (Stack term)
  , _binds :: Map binder term
  }
  deriving (Eq, Show)
makeLenses ''Memory

type MachineMemory = Memory Location Variable Term

instance (Ord b, Ord l) => Semigroup (Memory l b t) where
  m1 <> m2 =
    Memory
      { _stacks = m1 ^. stacks <> m2 ^. stacks
      , _binds = m1 ^. binds <> m2 ^. binds
      }

instance (Ord b, Ord l) => Monoid (Memory l b t) where
  mempty = Memory mempty mempty
