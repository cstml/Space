module Space.TypeChecking.Derivation where

import Space.Syntax
import Space.Syntax.Context

data Judgement = Judgement {_judgement :: (Context, Term, TermType)}
  deriving stock (Show,Eq)

data Derivation
  = AxiomD Judgement 
  | PushD  Judgement Derivation Derivation
  | PopD Judgement Derivation  
  | SequenceD Judgement Derivation Derivation
  deriving (Show,Eq)

