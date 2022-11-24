module Space.Evaluator where

import Control.Monad.Freer
import Control.Monad.Freer.State
import Control.Monad.Freer.TH

import Control.Lens 

import Text.Read  (readMaybe)
--import Data.Char 
import Data.Map ((!?), insert )
import qualified Data.Map as M 

import Space.Syntax.Types

data EvalEff a where
  PushTerm :: Location -> Term ->  EvalEff ()
  PopTerm :: Location -> EvalEff Term
  BindTerm :: Term -> Variable -> EvalEff ()
  GetBinding :: Variable -> EvalEff Term
  GetMemory :: EvalEff (Bindings,Stacks)
  PutMemory :: Bindings -> Stacks -> EvalEff ()
makeEffect ''EvalEff

toEff :: forall effs . Term -> Eff (EvalEff ': effs) Term
toEff = \case
  
  Closure t c -> do
    (initBindings, _) <- getMemory 
    toEff t
    (_, newStacks) <- getMemory
    putMemory initBindings newStacks
    toEff c 

  Variable v c -> do 
    t <- getBinding v 
    toEff (t <> c)
  
  Bind l v c -> do 
    t <- popTerm l 
    bindTerm t v 
    toEff c

  Push l t c -> do 
    pushTerm l t 
    toEff c 

  NoOp -> pure NoOp

runEvalEff :: Eff '[EvalEff] a -> Eff '[State Bindings, State Stacks, IO] a 
runEvalEff = reinterpret3  go
  where
    popPosition :: Location -> Eff '[State Bindings, State Stacks, IO] Term
    popPosition l = do
        st <- (view stacks) <$> get
        case st !? l of 
          Nothing -> pure NoOp
          Just (t:ts) -> do
            put (MkStacks $ insert l ts st)
            pure t 
          Just [] -> pure NoOp
    mustBeVariable = \case  
      Variable x NoOp -> x
      x -> error $ show x <> " <- must be a variable"
      
    mustBeInteger (MkVariable x) = maybe (error $ show x <> " <- must be of type integer") id $ readMaybe @Integer x

    doNumOp f = do
      t1 <- mustBeInteger . mustBeVariable <$> popPosition Spine
      t2 <- mustBeInteger . mustBeVariable <$> popPosition Spine
      pure $ Variable (MkVariable $ show $ f t1 t2) NoOp
      
      
    go :: EvalEff a -> Eff '[State Bindings, State Stacks, IO] a
    go = \case
      
      PushTerm l t ->  do
        modify
          (\(b :: Stacks) -> 
                case (b ^. stacks) !? l of  
                  Just ts -> MkStacks $ insert l (t : ts) (b^.stacks)
                  Nothing -> MkStacks $ insert l (t : []) (b^.stacks))

      PopTerm l -> popPosition l

      BindTerm t v -> modify (\(MkBindings bs) -> MkBindings $ insert v t bs )  

      GetBinding var@(MkVariable v) -> do 
        (MkBindings bs) <- get
        
        case readMaybe @Integer v of
          Just _ -> pure $ Push Spine (Variable var NoOp) NoOp
          
          Nothing ->
            case v of
            "+" -> doNumOp (+) >>= \t -> pure $ Push Spine t NoOp
            "-" -> doNumOp (-) >>= \t -> pure $ Push Spine t NoOp
            "*" -> doNumOp (*) >>= \t -> pure $ Push Spine t NoOp
            "/" -> doNumOp div >>= \t -> pure $ Push Spine t NoOp
            "%" -> doNumOp mod >>= \t -> pure $ Push Spine t NoOp
            _ -> case bs M.!? var of
              Just t -> pure t
              Nothing -> error $ "Unbound variable: " <> v

      GetMemory -> (,) <$> get <*> get

      PutMemory x y -> put x >> put y 

runEval :: Term -> IO ((Term, Bindings), Stacks)
runEval t = runM (runState (MkStacks M.empty) (runState (MkBindings M.empty) (runEvalEff (toEff t))))

