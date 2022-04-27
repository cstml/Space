module Space.Evaluator.Exception where

data MException
  = EvaluationError String
  | TypeMissmatch String
  deriving (Show, Eq)
