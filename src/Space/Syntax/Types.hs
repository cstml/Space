module Space.Syntax.Types where

import Data.String 
import Data.Map (Map)
import Control.Lens

newtype Variable = MkVariable String 
  deriving stock (Show,Eq)
  deriving newtype Ord 

instance IsString Variable where 
  fromString = MkVariable

data Location 
  = Spine 
  | Input
  | Output
  | Return
  deriving stock (Show,Eq, Ord)

data Term 
  = Closure Term Term 
  | Variable Variable Term
  | Bind Location Variable Term
  | Push Location Term Term 
  | NoOp
  deriving stock (Show,Eq)

instance Semigroup Term where
  NoOp <> a = a 
  c <> NoOp = c
  Closure t c <> a = Closure t (c <> a)
  Variable v c <> a = Variable v (c <> a) 
  Bind l v c <> a = Bind l v (c <> a)
  Push l t c <> a = Push l t (c <> a)

instance Monoid Term where
  mempty = NoOp

newtype Bindings = MkBindings { _bindings :: Map Variable Term }
  deriving stock (Show,Eq)

makeLenses ''Bindings 

newtype Stacks = MkStacks { _stacks :: Map Location [Term] }
  deriving stock  (Show,Eq)

makeLenses ''Stacks 
