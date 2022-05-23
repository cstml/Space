{-# LANGUAGE TypeOperators #-}

module Space.Evaluator.Implementation where

import Aux.Unfoldable
import Control.Lens
import Control.Monad.Freer
import Control.Monad.Freer.Error qualified as E
import Control.Monad.Freer.Reader qualified as R
import Control.Monad.Freer.State qualified as S
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import Data.Kind
import Data.String (fromString)
import Prettyprinter (pretty)
import Space.Evaluator.Exception
import Space.Evaluator.Machine
import Space.Evaluator.Memory
import Space.Language
import Space.Parser

type MachineMemory' = Memory Location Variable

newtype Environment = Environment ()
  deriving stock (Eq, Ord, Show)
  deriving newtype (Monoid, Semigroup)

toNum :: Term -> (Term, Maybe Int)
toNum t = case t of
  SInteger x SEmpty -> (t, pure x)
  _ -> (t, Nothing)

fromNum :: Int -> Term
fromNum = flip SInteger SEmpty

boolToInt x = if x then 1 else 0

type MStack = '[R.Reader Environment, S.State MachineMemory, E.Error MException, IO]

fEval :: Eff (EvalMachine ': MStack) w -> Eff MStack w
fEval = interpret $ \case
  GetMemoryM -> S.get
  PutMemoryM nMem -> S.put nMem
  OutputM term -> sendM $ print term
  InputM -> sendM $ fromString <$> readLn

fEval' :: Environment -> MachineMemory -> Eff MStack w -> IO (Either MException (w, MachineMemory))
fEval' env mem = runM . E.runError . S.runState mem . R.runReader env

fEval'' = fEval' mempty mempty

eStep :: Eff (StateMachine ': EvalMachine ': '[]) r -> Eff '[EvalMachine] r
eStep = interpret $ \case
  StepM term -> evaluate' term
   where
    evaluate' = \case
      SEmpty -> getMemoryM
      (unfold -> x : cons) ->
        let con = mconcat cons
         in case x of
              SInteger _ _ -> pushGammaM x >> evaluate' con
              SChar _ _ -> pushGammaM x >> evaluate' con
              SPop v l _ -> case l of
                Location "I" -> do
                  i <- inputM
                  case parseTerm i of
                    Left e -> errorM $ show e
                    Right t -> bind1M v t
                  evaluate' con
                _ -> (bind1M v =<< pop1M l) >> evaluate' con
              SPush t l _ -> case l of
                Location "O" -> do
                  case t of
                    -- if it is a variable, then get its bound value;
                    SVariable v SEmpty -> do
                      b <- getBindM v
                      outputM (pretty b)
                      evaluate' con
                _ -> push1M l t >> evaluate' con
              SVariable (Variable v) _ ->
                let op o = do
                      (ta, a) <- toNum <$> popGammaM
                      (tb, b) <- toNum <$> popGammaM
                      let res = o <$> a <*> b
                      case res of
                        Nothing ->
                          errorM $ show $ TypeMissmatch $ "Expected 2 Ints, got: " <> (show . pretty) ta <> " " <> (show . pretty) tb <> "."
                        Just res' -> pushGammaM (fromNum res') >> evaluate' con

                    eq o = do
                      t1 <- popGammaM
                      t2 <- popGammaM
                      let res = t1 `o` t2
                      pushGammaM (fromNum . boolToInt $ res) >> evaluate' con

                    execIf = do
                      ifTerm <- pop1M DLocation
                      evaluate' ifTerm
                      result <- popGammaM
                      case result of
                        SInteger 0 SEmpty -> do
                          _ <- pop1M DLocation
                          b <- pop1M DLocation
                          evaluate' (b <> con)
                        _ -> do
                          a <- pop1M DLocation
                          _ <- pop1M DLocation
                          evaluate' (a <> con)
                 in case v of
                      "if" -> execIf
                      "+" -> op (+)
                      "-" -> op (-)
                      "*" -> op (*)
                      "^" -> op (^)
                      "/" -> op div
                      "==" -> eq (==)
                      "/=" -> eq (/=)
                      "?" -> pop1M DLocation >>= evaluate' >> evaluate' con
                      "!" -> popGammaM >>= push1M DLocation >> evaluate' con
                      _ -> do
                        t <- getBindM $ Variable v
                        if t == x
                          then pushGammaM t >> evaluate' con
                          else evaluate' $ t <> con

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
            Location "I" -> do
              i <- input
              case parseTerm i of
                Left e -> error $ show e
                Right t -> bind1 v t
              evaluate con
            _ -> (bind1 v =<< pop1 l) >> evaluate con
          SPush t l _ -> case l of
            Location "O" -> do
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
