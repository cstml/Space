module Space.Aux.Evaluate where

class Evaluate mem term exception m  where
  eval :: mem -> term -> m  (Either exception mem)
