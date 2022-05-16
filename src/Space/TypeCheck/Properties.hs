module Space.TypeCheck.Properties where

import Space.TypeCheck.Exception
import Control.Monad.Identity
import GHC.Base

newtype ReduceM a = ReduceM {unReduce :: Either TCError a}

instance Functor ReduceM where
  fmap f x = ReduceM $ f <$> unReduce x

instance Applicative ReduceM where
  pure = ReduceM . pure
  (ReduceM f) <*> (ReduceM v) = ReduceM $ f <*> v

instance Monad ReduceM where
  (ReduceM m) >>= f = case m of
    Left err -> ReduceM . Left $ err
    Right x -> f x

instance MonadFail ReduceM where
  fail = ReduceM . Left . TCErrorString

class Reduce t where
  reduce1 :: t -> ReduceM t

class Normalise t where
  normalise :: t -> t
  
  (===) :: Eq t => t -> t -> Bool
  x === y = normalise x == normalise y
  
