module Space.Syntax.Types where

import Data.String 
import Data.Map (Map)
import Control.Lens
import Prettyprinter
import qualified Data.Map as M

data Expression
  = TermExp Term
  | TypeExp (Variable,TermType)
  deriving (Eq,Show)

newtype Variable = MkVariable String 
  deriving stock (Show,Eq)
  deriving newtype Ord 

instance IsString Variable where 
  fromString = MkVariable

instance Pretty Variable where
  pretty (MkVariable x) = pretty x 
  
data Location 
  = Spine 
  | Input
  | Output
  | Return
  deriving stock (Show,Eq, Ord)

instance Pretty Location where
  pretty = \case
    Spine -> "@_"
    Input -> "@I"
    Output -> "@O"
    Return -> "@^"

newtype Atom = MkAtom String
  deriving stock (Show,Eq)
  deriving newtype Ord 

instance IsString Atom where 
  fromString = MkAtom

instance Pretty Atom where
  pretty = viaShow

data TermType
  = TypeVar Atom
  | TypeConst Atom
  | TermType :->: TermType
  | TypeVector [TermType]
  | TypeLocation Location TermType 
  deriving stock (Show,Eq,Ord)

instance Pretty TermType where
  pretty = viaShow

instance Semigroup TermType where
  x <> y = case x of
    TypeVector xs -> case y of
      TypeVector ys -> TypeVector (xs <> ys)
      _ -> TypeVector $ xs <> [y]
    _ -> case y of 
      TypeVector ys -> TypeVector (x : ys)
      _ -> TypeVector [x, y]

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

instance Pretty Term where
  pretty = \case
    NoOp -> ""
    Closure t NoOp -> braces  (pretty t)
    Closure t c -> braces  (pretty t) <+> pretty c
    Variable v NoOp -> pretty v
    Variable v c -> pretty v <+> pretty c
    Bind l v NoOp -> angles (pretty v) <+> pretty l
    Bind l v c -> angles (pretty v) <+> pretty l <+> pretty c
    Push l t NoOp -> brackets (pretty t) <+> pretty l
    Push l t c -> brackets (pretty t) <+> pretty l  <+> pretty c

newtype Bindings = MkBindings { _bindings :: Map Variable Term }
  deriving stock (Show,Eq)
  deriving newtype (Semigroup, Monoid)

makeLenses ''Bindings 

instance Pretty Bindings where
  pretty (MkBindings b) = list $ f <$> l
    where
      l = M.toList b
      f (b,t) = pretty b <+> "=" <+> pretty t

newtype Stacks = MkStacks { _stacks :: Map Location [Term] }
  deriving stock  (Show,Eq)
  deriving newtype (Semigroup, Monoid)

makeLenses ''Stacks

instance Pretty Stacks where
  pretty (MkStacks s) = list $ f <$> l
    where
      l = M.toList s
      f (b,t) = pretty b <+> "=" <+> pretty t
