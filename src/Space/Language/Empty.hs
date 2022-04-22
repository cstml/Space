module Space.Language.Empty where

import qualified Data.Void as D
import Space.Aux.PShow

{- Empty here is equivalent in the logical sense with the non-inhabited Void. The
 issue is that we cannot represent (or Show) Void easily ~ or I haven't found a
 way to do so. So I will be using a drop in place replacement.

type Empty = Void

-}

newtype Empty = Empty {getEmpty :: D.Void}

instance Show Empty where
  show = \case
    Empty _ -> "Void"

instance PShow Empty where
  pShow = \case
    _ -> "Void"

type Void = Empty
