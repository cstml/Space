{-# LANGUAGE AllowAmbiguousTypes #-}

module Space.Evaluator.Stack where

import Control.Lens
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Sequence

newtype Stack a = Stack {_stack :: Seq a}
  deriving stock (Eq, Show)
  deriving newtype (Semigroup, Monoid)

makeLenses ''Stack
