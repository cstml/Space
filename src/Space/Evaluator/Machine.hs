{-# LANGUAGE DataKinds #-}
{-# Language KindSignatures #-}
{-# Language ConstraintKinds #-}
{-# Language TemplateHaskell #-}
-- {-# Language PolyKinds #-}
{-# Language LambdaCase #-}
{-# Language ExplicitForAll #-}
{-# Language MultiParamTypeClasses#-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# Language DerivingStrategies #-}
{-# Language GeneralisedNewtypeDeriving #-}
{-# Language ScopedTypeVariables#-}
-- {-# LANGUAGE InstanceSigs #-}
-- {-# Language TypeSynonymInstances #-}
{-# Language FlexibleInstances #-}
{-# Language FlexibleContexts #-}
{-# Language FunctionalDependencies #-}
{-# Language TypeOperators #-}
{-# Language TypeApplications #-}
module Space.Evaluator.Machine where

import Space.Language
import Space.Evaluator.Memory
import Space.Evaluator.Stack

import Control.Monad.Trans.Reader
import Control.Monad.Reader
import Control.Monad.State

import Control.Lens hiding ((<|),(:<), Empty)

import Data.Map ( Map )
import qualified Data.Map as Map
import Data.Sequence

import Data.Kind
import Space.Language (Variable, Term (SInteger))
import System.IO.Error.Lens (location)
import Data.Maybe (fromMaybe)

newtype Environment = Environment ()

type SMachine a = ReaderT Environment (State Memory) a

class Weird (c :: Type -> Constraint) where

instance Weird Eq where

class (MonadFail mac) => Machine (mac :: Type -> Type) (mem :: Type) (var :: Type) (location :: Type)
    | mac -> mem
    , mac -> var
    , mac -> location
    where
    getMemory :: mac mem
    putMemory :: mem -> mac ()
    updateMemory :: (mem -> mem) -> mac ()
    putStack :: location -> Stack -> mac ()
    pop1 ::  location -> mac Term
    pop1Bind :: var -> location -> mac Term
    bind1 :: var -> Term -> mac ()
    push1 :: location -> Term -> mac ()
    run :: mac () -> mem

instance Machine (ReaderT Environment (State Memory)) Memory Variable Location where
    getMemory = get

    putMemory = put

    updateMemory f = getMemory >>= putMemory . f

-- putStack :: Location -> Stack -> SMachine ()
    putStack l s = getMemory >>= putMemory . (stacks . at l . _Just .~ s)

-- push1 :: Location -> Term -> SMachine ()
    push1 l t = do
        mem <- getMemory
        let a :: Maybe Stack = mem ^. stacks . at l
        let b = case a of
                    Just (Stack s) ->  review stack $ t <| s
                    Nothing -> review stack $ singleton t
        let nMem = mem & stacks . at l ?~ b
                --Just s ->  mem & (stacks . at l) ?~ (Just . Stack $ t <| s)
                --Nothing -> mem
        putMemory nMem

-- pop1 ::  Location -> SMachine Term
    pop1 l = do
        m <- getMemory
        case viewl $ view (stacks . ix l . stack) m of
            x :< xs -> putStack l (review stack xs) >> pure x
            EmptyL -> pure SEmpty

    run = flip execState mempty . flip runReaderT (Environment ())
-- bind1 :: Variable -> Term -> SMachine ()
    bind1 v t = do
        m <- getMemory
        putMemory $ m & binds . ix v .~ t

    -- pop1Bind :: Variable -> Location -> SMachine Term
    pop1Bind v l = do
        t <- pop1 l
        bind1 v t
        return t

exMem :: Memory
exMem = Memory mempty (Map.fromList [(Variable "x",SEmpty)])

type Terms = [Term]

toNum :: Term -> Int
toNum = \case 
    SInteger x -> x
    _ -> error "Not A number"
fromNum = SInteger

evaluate :: (Machine m mem v Location) => Terms -> m mem
evaluate = \case
    [] -> getMemory
    (x : xs) -> case x of 
        (SInteger _) -> push1 (Location "Ho") x >> evaluate xs
        (SVariable (Variable v)) -> 
            let 
                op o = do
                    a <- toNum <$> pop1 (Location "Ho")
                    b <- toNum <$> pop1 (Location "Ho")
                    push1 (Location $  "Ho") (fromNum $ a `o` b)
                    evaluate xs
            in case v of
                "+" -> op (+)
                "*" -> op (*)
                "^" -> op (^)
                "/" -> op div


    _ -> fail "Not Complteted "

evaluate' :: Terms -> Memory
evaluate' t = flip execState mempty . flip runReaderT (Environment ())  $ evaluate t