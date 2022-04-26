module Space.Evaluator.Exception where

data MException
  = EvaluationError String
  | WrongType String
  deriving (Show, Eq)
