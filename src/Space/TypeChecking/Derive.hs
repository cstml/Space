{-# OPTIONS_GHC -Wno-unused-matches #-}
module Space.TypeChecking.Derive where

import Data.Map
import Space.TypeChecking.Derivation
import Space.TypeChecking.Context
import Space.Syntax
import Control.Monad.Identity
import Control.Monad.Freer.Error
import Control.Monad.Freer.State
import Control.Monad.Freer.Reader
import Control.Monad.Freer.Writer
import Control.Monad.Freer

newtype Constraint = Constraint (TermType, TypeOp)
  deriving stock (Show,Eq)

infixr 8 :+: 
infixr 8 :-: 

data TypeOp
  = TypeOp :+: TypeOp
  | TypeOp :-: TypeOp
  | T TermType
  deriving stock (Show,Eq)

newtype DeriverErr = Err String
  deriving stock (Show,Eq)

type Deriver a = forall eff . Members '[Writer [Constraint], Reader Context, State [Atom], Error DeriverErr] eff => Eff eff a


{-

------------------ NoOp
   G |- {} : {}


       G |- x : t 
----------------------- Push
 G |- [x]@l y : {t}@l 


    G, x:k |-  y : x1@(t)
--------------------------- Pop/Bind
 G |- <x>@l  y : k@l -> t



G |- x : t -> x1@(j + y)       G |- y : x2(j + z) -> k
-------------------------------------------------------- Composition
         G |- x y :  x3@(t + z) -> x4@(y + k)

x1 = j + y
x2 = j + z
x3 = x + z
x4 = y + k  
-}

derive :: Term -> Deriver Derivation 
derive term = do
  context <- ask 
  case term of 
    Closure t1 t2 -> undefined
    
    Variable v t -> do
      t1 <- getContextType v
      dc <- derive t
      pure $ AxiomD (Judgement (context, NoOp, TypeVector [])) dc
    
    Bind l v t -> do
      tf <- TypeVar <$> fresh
      tPrev <- TypeVar <$> fresh
      let
        Context ctx addCtx = context 
        newContext = Context ctx ((v,tf) : addCtx)
      dc <- local (const newContext) $ derive t
      let
        tc = getDerivationType dc
      tell [Constraint(tc,T tPrev :-: T tf)]
      pure $ PopD (Judgement (newContext, term, tf)) dc
      
    Push l tp tc -> do
      tf <- TypeVar <$> fresh
      d1 <- derive tp
      d2 <- derive tc
      let
        t1 = getDerivationType d1
        t2 = getDerivationType d2
      tell [Constraint (tf,T (TypeLocation l t1) :+: T t2)]
      pure $ PushD (Judgement (context, term, tf)) d1 d2
    
    NoOp -> do 
      pure $ EndD $ Judgement (context, NoOp, TypeVector [])
      
  where
    fresh :: Deriver Atom
    fresh = do
      r <- get
      case r of
        (x:xs) -> put xs >> return x
        _ -> throwError $ Err "Infinite stream ended"
    getDerivationType = \case
      AxiomD j _ -> getJudgementType j
      PushD j _ _ -> getJudgementType j
      PopD j _ -> getJudgementType j
      SequenceD j _ _ -> getJudgementType j
      
    getJudgementType :: Judgement -> TermType
    getJudgementType (Judgement (_,_,t)) = t 

    getContext :: Derivation -> Context
    getContext = undefined

    getContextType = undefined 
