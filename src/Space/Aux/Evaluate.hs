module Space.Aux.Evaluate where

import Prettyprinter

class Evaluate mem term exception m  where
  eval :: mem -> term -> m (Either exception mem)
