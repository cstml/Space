module Space.Evaluator.Implementation.Pure where

import Aux.Unfoldable
import Control.Arrow
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
import Space.Aux.Evaluate
import Space.Evaluator.Exception
import Space.Evaluator.Implementation
import Space.Evaluator.Machine
import Space.Evaluator.Memory
import Space.Evaluator.Stack
import Space.Language
import Space.Parser
