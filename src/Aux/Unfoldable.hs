module Aux.Unfoldable where

class Unfoldable a where
  unfold :: a -> [a]
