module Space.TypeChecking.Context where

import Space.Syntax.Types
import Data.Map

data Context = Context { context :: [(Variable, TermType)]
                       , addContext :: [(Variable, TermType)]
                       }
  deriving (Show,Eq)

instance Semigroup Context where
  (Context a b) <> (Context c d) = Context (a <> c) (b <> d)

instance Monoid Context where
  mempty = Context mempty mempty
