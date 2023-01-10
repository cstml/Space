module Space.Syntax.Context where

import Space.Syntax.Types
import Data.Map

data Context = Context { context :: [(Variable, TermType)]
                       , addContext :: [(Variable, TermType)]
                       }
  deriving (Show,Eq)
