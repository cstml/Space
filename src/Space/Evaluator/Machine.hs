{-# Language TemplateHaskell #-}
{-# Language LambdaCase #-}

module Space.Evaluator.Machine where

import Space.Language
import Space.Evaluator.Memory
import Space.Evaluator.Stack

import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.Trans.Class

import Control.Lens hiding ((<|),(:<))

import Data.Map ( Map )
import qualified Data.Map as Map
import Data.Sequence

newtype Environment = Environment ()
type SMachine a = ReaderT Environment (State Memory) a

getMemory :: SMachine Memory
getMemory = lift get

putMemory :: Memory -> SMachine ()
putMemory = lift . put

updateMemory :: ( Memory -> Memory ) -> SMachine ()
updateMemory f = getMemory >>= putMemory . f

putStack :: Location -> Stack -> SMachine ()
putStack l s = getMemory >>= putMemory . (stacks . at l . _Just .~ s)

push1 :: Location -> Term -> SMachine ()
push1 l t = updateMemory $ stacks . at l . _Just . stack %~ (t <|)

pop1 ::  Location -> SMachine Term
pop1 l = do
    m <- getMemory
    case viewl $ view (stacks . ix l . stack) m of
        x :< xs -> putStack l (review stack xs) >> pure x
        EmptyL -> pure SEmpty

bind1 :: Variable -> Term -> SMachine ()
bind1 v t = do
    m <- getMemory
    putMemory $ m & binds . ix v .~ t

pop1Bind :: Variable -> Location -> SMachine Term
pop1Bind v l = do
    t <- pop1 l
    bind1 v t
    return t

evaluate :: Term -> SMachine ()
evaluate t = case t of
    SInteger _ -> push1 (Location "Home") t

evaluate' :: Term -> Memory
evaluate' t = flip execState mempty . flip runReaderT (Environment ()) $ evaluate t