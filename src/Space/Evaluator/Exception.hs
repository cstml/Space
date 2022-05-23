module Space.Evaluator.Exception where

import Control.Exception
import Prettyprinter

data MException
  = EvaluationError String
  | TypeMissmatch String
  deriving (Show, Eq, Exception)

instance Pretty MException where
  pretty = unsafeViaShow
