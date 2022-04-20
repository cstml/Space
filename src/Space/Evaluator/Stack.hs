{-# LANGUAGE DataKinds #-}
{-# Language KindSignatures #-}
{-# Language ConstraintKinds #-}
{-# Language TemplateHaskell #-}
-- {-# Language PolyKinds #-}
{-# Language LambdaCase #-}
{-# Language ExplicitForAll #-}
{-# Language MultiParamTypeClasses#-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# Language DerivingStrategies #-}
{-# Language GeneralisedNewtypeDeriving #-}
{-# Language ScopedTypeVariables#-}
-- {-# LANGUAGE InstanceSigs #-}
-- {-# Language TypeSynonymInstances #-}
{-# Language FlexibleInstances #-}
{-# Language FlexibleContexts #-}
{-# Language FunctionalDependencies #-}
{-# Language TypeOperators #-}
{-# Language TypeApplications #-}

module Space.Evaluator.Stack where

import Space.Language

import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.Trans.Class

import Control.Lens hiding ((<|),(:<))

import Data.Map ( Map )
import qualified Data.Map as Map
import Data.Sequence

newtype Stack = Stack { _stack :: Seq Term }
    deriving newtype (Eq,Show, Semigroup, Monoid)
makeLenses ''Stack