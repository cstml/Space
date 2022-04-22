module Space.Language.Term where

import Space.Aux.PShow
import Space.Language.Empty (Void)
import Space.Language.Location
import Space.Language.Type
import Space.Language.Variable
import Space.Language.Vector

{- This sort of AST is the tag specifc one used in my previous project. Will
 attempt at a tagless initial together with a tagless final implementation.

More on the two techniques can be found here:

- https://serokell.io/blog/introduction-tagless-final.
-}
data Term
  = SVariable Variable
  | SInteger Int
  | SChar Char
  | SPush Term Location
  | SPop Variable Location
  | SEmpty
  deriving (Eq, Show, Ord)

{-
data Term a where
  SVar  ::   Variable -> Term a -> Term (Vector Variable a)
  SInt  ::  Int -> Term a -> Term (Vector Int a)
  SChar :: Char ->    Term a ->    Term (Vector Char a)
  SPush :: Location l (Term a) -> Term b ->  Term (Vector (Location l (Term a)) b)
  SPop  ::  (Show a, PShow c, Show c ) =>
    Term (Vector (Location l (Term a)) c) ->
    Location l (Term (Vector Variable Void)) ->
    Term d ->
    Term (Vector c d)
  SEmpty ::
    Term Void

instance Show (Term a) where
  show =
    let sep = " "
        bracket x = "(" <> x <> ")"
     in \case
          SVar var con -> bracket $ "SVar " <> show var <> sep <> show con
          SInt int con -> bracket $ "SInt " <> show int <> sep <> show con
          SChar ch con -> bracket $ "SChar " <> show ch <> sep <> show con
          SPush loc con -> bracket $ "SPush " <> bracket (show loc) <> show con
          SPop var loc con -> bracket $ "SPop " <> show var <> show loc <> show con
          SEmpty -> "SEmpty"
-}
{-
instance Eq (Term a) where
  (==) = \case
    SVar var con -> \case
      SVar var' con' -> var == var' && con == con'
      _ -> False
    SInt int con -> \case
      SInt int' con' -> int == int' && con == con'
      _ -> False
    SChar ch con -> \case
      SChar ch' con' -> (ch,con) == (ch',con')
      _ -> False
    SPush loc con -> \case
      SPush loc' con' -> (loc,con) == (loc',con')
      _ -> False
    SPop t loc con -> \case
      SPop t' loc' con' -> (t,loc,con) == (t',loc',con')
      _ -> False
    SEmpty -> \case
      SEmpty -> True
      _ -> False
-}

{-
instance PShow (Term a) where
  pShow =
    let sep = " "
        cur x = "{" <> x <> "}"
        ang x = "<" <> x <> ">"
        squ x = "[" <> x <> "]"
     in \case
          SVar var con -> pShow var <> sep <> pShow con
          SInt int con -> show int <> sep <> pShow con
          SChar ch con -> show ch <> sep <> pShow con
          SPop var loc con -> ang (pShow var) <> pShow loc <> pShow con
          SPush loc con -> pShow loc <> pShow con
          SEmpty -> "*"
-}
{-
ex1 :: Term (Vector Char Void)
ex1 = SChar 'x' SEmpty

ex2 :: Term (Vector Char (Vector Char Void))
ex2 = SChar 'x' $ SChar 'y' SEmpty

{-
  SPush ::
    (Show b) =>
    Location l (Term a) -> Term b -> Term (Vector (Location l (Term a)) b)
-}

ex39 = LC (SVar (Variable "x") SEmpty)

ex30 = SInt 1 SEmpty

ex31 = SPush (LC (SInt 1 SEmpty)) SEmpty

ex32 = SPop ex31 ex39 SEmpty
-}
