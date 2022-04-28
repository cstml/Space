module Space.Evaluator.Exception where

import Prettyprinter

data MException
  = EvaluationError String
  | TypeMissmatch String
  deriving (Show, Eq)

instance Pretty MException where
  pretty = unsafeViaShow
