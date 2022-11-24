module Space.Evaluator where

import Control.Monad.Freer
import Control.Monad.Freer.State
import Control.Monad.Freer.TH

import Control.Lens

import Space.Syntax.Parser
import Control.Monad.Freer.Error

import Text.Read  (readMaybe)
import Data.Map ((!?), insert )
import qualified Data.Map as M
import Data.Maybe

import Space.Syntax.Types

data EvalError
  = EvalError String
  -- | Wrong type encountered.
  | TypeMismatch String
  -- | Stack underflow. 
  | StackUndeflow String
  -- | Cannot parse the term.
  | ParseError String
  -- | Cannot pop from Output.
  | ErrPopOutput
  | UnboundVariable String
  deriving stock (Show,Eq)

type MachineMemory = (Bindings,Stacks)

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

runEvalEff :: Eff '[EvalEff] a -> Eff '[State MachineMemory, Error EvalError, IO] a
runEvalEff = reinterpret3 go
  where
    popPosition :: Location -> Eff '[State MachineMemory, Error EvalError, IO] Term
    popPosition l = do
        (bi :: Bindings ,MkStacks st) <- get
        case st !? l of
          Nothing -> pure NoOp
          Just (t:ts) -> do
            put (bi,MkStacks $ insert l ts st)
            pure t
          Just [] -> throwError $ StackUndeflow $ "error: Stack underflow location: " <> show l
            -- pure NoOp


    mustBeVariable = \case
      Variable x NoOp -> pure x
      x -> throwError $ TypeMismatch $ show x <> " <- must be a variable"

    mustBeInteger (MkVariable x) =
      maybe (throwError $ TypeMismatch $ show x <> " <- must be of type integer") pure $ readMaybe @Integer x

    primitive t = do
      isJust $ readMaybe @Integer t
      || (case t of
             "true" -> True
             "false" -> True
             _ -> False
          )
    doNumOp :: (Integer -> Integer -> Integer) -> Eff '[State (Bindings, Stacks),Error EvalError , IO] Term
    doNumOp f = do
      t1 <- mustBeInteger =<< mustBeVariable =<< popPosition Spine
      t2 <- mustBeInteger =<< mustBeVariable =<< popPosition Spine
      pure $ Variable (MkVariable $ show $ f t1 t2) NoOp

    go :: EvalEff a -> Eff '[State (Bindings, Stacks),Error EvalError , IO] a
    go = \case

      PushTerm l t ->  do
        modify
          (\(a :: Bindings ,b :: Stacks) ->
                case (b ^. stacks) !? l of
                  Just ts -> (a,MkStacks $ insert l (t : ts) (b^.stacks))
                  Nothing -> (a,MkStacks $ insert l [t] (b^.stacks)))

      PopTerm l -> case l of
        Input -> do
          inp <- sendM $ readLn @String
          either (throwError . ParseError . show) pure $ parseTerm inp
        Output -> throwError ErrPopOutput
        _ -> popPosition l

      BindTerm t v -> modify (\(MkBindings bs, x :: Stacks) -> (MkBindings $ insert v t bs,x))

      GetBinding var@(MkVariable v) -> do
        (MkBindings bs, _) :: (Bindings,Stacks) <- get

        if primitive v
          then pure $ Push Spine (Variable var NoOp) NoOp
          else case v of
            "+" -> doNumOp (+) >>= \t -> pure $ Push Spine t NoOp
            "-" -> doNumOp (-) >>= \t -> pure $ Push Spine t NoOp
            "*" -> doNumOp (*) >>= \t -> pure $ Push Spine t NoOp
            "/" -> doNumOp div >>= \t -> pure $ Push Spine t NoOp
            "%" -> doNumOp mod >>= \t -> pure $ Push Spine t NoOp
            "if" -> do
              t1 <- popPosition Return
              t2 <- popPosition Return
              t3 <- popPosition Return
              runEvalEff $ toEff t1
              t1R <- popPosition Spine
              case t1R of
                Variable "true" NoOp -> runEvalEff $ toEff t2
                Variable "false" NoOp -> runEvalEff $ toEff t3
                x -> throwError $ TypeMismatch $ show x <> " not boolean."

            _ -> case bs M.!? var of
              Just t -> pure t
              Nothing -> throwError $ UnboundVariable v

      GetMemory -> get

      PutMemory x y -> put (x,y)

evalTerm :: Term -> IO (Either EvalError (Term, (Bindings, Stacks)))
evalTerm t = runM (runError (runState (MkBindings M.empty, MkStacks M.empty) (runEvalEff (toEff t))))

runEval :: Term -> IO (Term, (Bindings, Stacks))
runEval t = do
  res <- runM (runError (runState (MkBindings M.empty, MkStacks M.empty) (runEvalEff (toEff t))))
  case res of
    Right x -> pure x
    Left e -> error $ show e

runEval' ::  (Bindings, Stacks) -> Term -> IO (Either EvalError (Term, (Bindings, Stacks)))
runEval' mem t = runM (runError (runState mem (runEvalEff (toEff t))))
