module Space.Evaluator.Stack where

import Control.Lens
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Data.Sequence qualified as S
import Prettyprinter 

newtype Stack a = Stack {_stack :: S.Seq a}
  deriving stock (Eq, Show)
  deriving newtype (Semigroup, Monoid, Foldable)

instance (Show a, Pretty a) => Pretty (Stack a) where 
  pretty = foldr (\a doc -> pretty a <+> pipe <+> doc ) emptyDoc
makeLenses ''Stack
