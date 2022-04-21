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

data Memory = Memory
  { _stacks :: Map Location (Stack ())
  , _binds :: Map Variable (Term Void)
  }
  deriving (Eq, Show)

makeLenses ''Memory

instance Semigroup Memory where
  m1 <> m2 =
    Memory
      { _stacks = m1 ^. stacks <> m2 ^. stacks
      , _binds = m1 ^. binds <> m2 ^. binds
      }

instance Monoid Memory where
  mempty = Memory mempty mempty
