module Space.TypeChecking.Derivation where

import Space.Syntax
import Space.TypeChecking.Context ( Context )

newtype Judgement = Judgement {_judgement :: (Context, Term, TermType)}
  deriving stock (Show,Eq)

data Derivation
  = AxiomD Judgement Derivation
  | PushD  Judgement Derivation Derivation
  | PopD Judgement Derivation  
  | SequenceD Judgement Derivation Derivation
  | EndD Judgement
  deriving stock (Show,Eq)

