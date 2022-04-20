{-# Language TemplateHaskell #-}

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
    deriving (Eq,Show)
makeLenses ''Stack