module Space.Language.Location where

import Space.Aux.PShow

newtype Location = Location String
  deriving (Eq, Show, Ord)

instance PShow Location where
  pShow (Location s) = "@" <> s
