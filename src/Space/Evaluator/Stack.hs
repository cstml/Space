{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE InstanceSigs #-}
-- {-# Language TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
-- {-# Language PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Space.Evaluator.Stack where

import Control.Lens hiding ((:<), (<|))
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Sequence
import Space.Language

{-

Due to encoding the Terms as continuations this term now becomes obsolete.

newtype Stack = Stack {_stack :: Seq Term }
  deriving newtype (Eq, Show, Semigroup, Monoid)

makeLenses ''Stack
-}

type Stack a = Term a
