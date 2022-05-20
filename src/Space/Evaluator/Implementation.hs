module Space.Evaluator.Implementation where

import Aux.Unfoldable
import Control.Lens
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import Data.Kind
import Prettyprinter (pretty)
import Space.Evaluator.Exception
import Space.Evaluator.Machine
import Space.Evaluator.Memory
import Space.Language
import Space.Parser

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
  ( EvaluationMachine
      (ReaderT Environment (ExceptT MException (StateT MachineMemory m)))
      MachineMemory
      Variable
      Location
      m
  ) =>
  Term ->
  ReaderT Environment (ExceptT MException (StateT MachineMemory m)) MachineMemory
evaluate = \case
  SEmpty -> getMemory
  (unfold -> x : cons) ->
    let con = mconcat cons

        pushGamma t = updateMemory (\mem -> mem & spine %~ (t <>))

        popGamma :: ReaderT Environment (ExceptT MException (StateT MachineMemory m)) Term
        popGamma = do
          mem <- getMemory
          case unfold (mem ^. spine) of
            x : xs -> putMemory (mem & spine .~ mconcat xs) >> return x
            _ -> return SEmpty
     in case x of
          SInteger _ _ -> pushGamma x >> evaluate con
          SChar _ _ -> pushGamma x >> evaluate con
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
                  (ta, a) <- toNum <$> popGamma
                  (tb, b) <- toNum <$> popGamma
                  let res = o <$> a <*> b
                  case res of
                    Nothing ->
                      lift . throwE $ TypeMissmatch $ "Expected 2 Ints, got: " <> (show . pretty) ta <> " " <> (show . pretty) tb <> "."
                    Just res' -> pushGamma (fromNum res') >> evaluate con

                eq o = do
                  t1 <- popGamma
                  t2 <- popGamma
                  let res = t1 `o` t2
                  pushGamma (fromNum . boolToInt $ res) >> evaluate con

                execIf = do
                  ifTerm <- pop1 DLocation
                  evaluate ifTerm
                  result <- popGamma
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
                  "?" -> pop1 DLocation >>= evaluate >> evaluate con
                  "!" -> popGamma >>= push1 DLocation >> evaluate con
                  _ -> do
                    t <- getBind $ Variable v
                    if t == x
                      then pushGamma t >> evaluate con
                      else evaluate $ t <> con
