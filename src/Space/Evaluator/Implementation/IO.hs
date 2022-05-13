module Space.Evaluator.Implementation.IO where

import Aux.Unfoldable
import Control.Lens hiding (Empty, (:<), (<|))
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import Data.Kind
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Sequence
import Data.String
import Prettyprinter (pretty)
import Space.Evaluator.Exception
import Space.Evaluator.Machine
import Space.Evaluator.Memory
import Space.Evaluator.Stack
import Space.Language
import Space.Parser

type MachineMemory = Memory Location Variable Term

--type MachineStack = Stack Term

newtype Environment = Environment ()

type SMachine a = ReaderT Environment (ExceptT MException (StateT MachineMemory IO)) a

instance
  EvaluationMachine
    (ReaderT Environment (ExceptT MException (StateT MachineMemory IO)))
    MachineMemory
    Variable
    Location
    IO
  where
  output = liftIO . print
  getMemory = get
  putMemory = put
  updateMemory f = getMemory >>= putMemory . f
  putStack l s = getMemory >>= putMemory . (stacks . ix l .~ s)

  -- push1 :: Location -> Term -> SMachine ()
  push1 l t = do
    mem <- getMemory
    let b = case mem ^. stacks . at l of
          Just (Stack s) -> review stack $ t <| s
          Nothing -> review stack $ singleton t
    let nMem = mem & stacks . at l ?~ b
    putMemory nMem

  -- pop1 ::  Location -> SMachine Term
  pop1 l = do
    m <- getMemory
    case viewl $ m ^. stacks . ix l . stack of
      x :< xs -> do
        case xs of
          -- if it was the last element in the location we clear the location
          -- alltogether
          Empty -> do
            putMemory (m & stacks . at l .~ Nothing) >> pure x
          -- otherwsie we act normally
          _ -> do
            putStack l (review stack xs) >> pure x
      EmptyL -> do
        putMemory (m & stacks . at l .~ Nothing)
        pure SEmpty

  -- bind1 :: Variable -> Term -> SMachine ()
  bind1 v t = do
    m <- getMemory
    putMemory $ m & binds . at v ?~ t

  pop1Bind v l = do
    t <- pop1 l
    bind1 v t
    return t

  getBind v = do
    mem <- getMemory
    case mem ^. binds . at v of
      Just t -> pure t
      Nothing -> pure $ SVariable v SEmpty

  --  input  = fromString <$> (lift . lift . lift $  getLine)
  input = fromString <$> liftIO getLine

  run state = flip runReaderT (Environment ()) >>> runExceptT >>> flip runStateT state

toNum :: Term -> (Term, Maybe Int)
toNum t = case t of
  SInteger x SEmpty -> (t, pure x)
  _ -> (t, Nothing)

fromNum :: Int -> Term
fromNum = flip SInteger SEmpty

evaluate ::
  (EvaluationMachine (ReaderT Environment (ExceptT MException (StateT MachineMemory IO))) MachineMemory Variable Location IO) =>
  Term ->
  ReaderT Environment (ExceptT MException (StateT MachineMemory IO)) MachineMemory
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

                -- if it is anything else, then output the term itself
                _ -> output (pretty t) >> evaluate con
            _ -> push1 l t >> evaluate con
          SVariable (Variable v) _ ->
            let op o = do
                  (ta, a) <- toNum <$> pop1 DLocation
                  (tb, b) <- toNum <$> pop1 DLocation
                  let res = o <$> a <*> b
                  case res of
                    Nothing ->
                      lift . throwE $ TypeMissmatch $ "Expected 2 Ints, got: " <> show ta <> " " <> show tb
                    Just res' ->
                      push1 DLocation (fromNum res') >> evaluate con
             in case v of
                  "+" -> op (+)
                  "-" -> op (-)
                  "*" -> op (*)
                  "^" -> op (^)
                  "/" -> op div
                  _ -> do
                    t <- getBind $ Variable v
                    if t == x
                      then push1 DLocation t >> evaluate con
                      else evaluate $ t <> con

(>>>) = flip (.)

eval :: MachineMemory -> Term -> IO (Either MException MachineMemory)
eval mem term = do
  res <- run mem . void . evaluate $ term
  return $ go res
 where
  go = \case
    (Left e, _) -> Left e
    (Right (), mem) -> Right mem

evaluate' :: Term -> IO (Either MException MachineMemory)
evaluate' term = do
  result <- eval mempty term
  print $ pretty <$> result
  pure result
