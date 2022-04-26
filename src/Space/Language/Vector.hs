module Space.Language.Vector where

import Space.Language.Empty (Void)

data Vector a b where
  (:+:) :: a -> b -> Vector a b
  PureV :: a -> Vector a Void
  EmptyVector :: Vector Void Void

instance (Show a, Show b) => Show (Vector a b) where
  show = \case
    a :+: b -> show a <> " :+: " <> show b
    PureV a -> "PureV " <> show a
    EmptyVector -> "EmptyVector"
