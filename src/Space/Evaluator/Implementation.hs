module Space.Evaluator.Implementation where

import Aux.Unfoldable

import Space.Language
import Space.Evaluator.Machine
import Space.Evaluator.Memory
import Space.Evaluator.Exception
import Space.Parser



import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import Control.Monad.Identity
import Prettyprinter (pretty)

import Data.Kind

type MachineMemory = Memory Location Variable Term
newtype Environment = Environment ()


toNum :: Term -> (Term, Maybe Int)
toNum t = case t of
  SInteger x SEmpty -> (t, pure x)
  _ -> (t, Nothing)

fromNum :: Int -> Term
fromNum = flip SInteger SEmpty

boolToInt x = if x then 1 else 0

evaluate ::
  forall m.
  Monad m =>
  (EvaluationMachine (ReaderT Environment (ExceptT MException (StateT MachineMemory m))) MachineMemory Variable Location m) =>
  Term ->
  ReaderT Environment (ExceptT MException (StateT MachineMemory m)) MachineMemory
evaluate = \case
  SEmpty -> getMemory
  (unfold -> x : cons) ->
    let con = mconcat cons
     in case x of
          SInteger _ _ -> push1 DLocation x >> evaluate con
          SChar _ _ -> push1 DLocation x >> evaluate con
          SPop v l _ -> case l of
            Location "In" -> do
              i <- input
              case parseTerm i of
                Left e -> error $ show e
                Right t -> bind1 v t
              evaluate con
            _ -> (bind1 v =<< pop1 l) >> evaluate con
          SPush t l _ -> case l of
            Location "Ou" -> do
              case t of
                -- if it is a variable, then get its bound value;
                SVariable v SEmpty -> do
                  b <- getBind v
                  output (pretty b)
                  evaluate con
            _ -> push1 l t >> evaluate con
          SVariable (Variable v) _ ->
            let op o = do
                  (ta, a) <- toNum <$> pop1 DLocation
                  (tb, b) <- toNum <$> pop1 DLocation
                  let res = o <$> a <*> b
                  case res of
                    Nothing ->
                      lift . throwE $ TypeMissmatch $ "Expected 2 Ints, got: " <> (show . pretty) ta <> " " <> (show . pretty) tb <> "."
                    Just res' -> push1 DLocation (fromNum res') >> evaluate con
                eq o = do
                  t1 <- pop1 DLocation
                  t2 <- pop1 DLocation
                  let res = t1 `o` t2
                  push1 DLocation (fromNum . boolToInt $ res) >> evaluate con

                execIf = do
                  term <- pop1 DLocation
                  evaluate term
                  result <- pop1 DLocation
                  case result of
                    SInteger 0 SEmpty -> do
                      _ <- pop1 DLocation
                      b <- pop1 DLocation
                      evaluate (b <> con)
                    _ -> do
                      a <- pop1 DLocation
                      _ <- pop1 DLocation
                      evaluate (a <> con)
             in case v of
                  "if" -> execIf
                  "+" -> op (+)
                  "-" -> op (-)
                  "*" -> op (*)
                  "^" -> op (^)
                  "/" -> op div
                  "==" -> eq (==)
                  "/=" -> eq (/=)
                  _ -> do
                    t <- getBind $ Variable v
                    if t == x
                      then push1 DLocation t >> evaluate con
                      else evaluate $ t <> con

