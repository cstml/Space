module Test.Space.Evaluator.Machine where

import Control.Lens
import Data.Map qualified as M
import Data.Sequence qualified as S
import Space
import Test.Tasty
import Test.Tasty.HUnit

ok :: forall a b. a -> Either b a
ok = pure

err :: forall b. MException -> Either MException b
err = Left

test1 =
  testGroup
    "Evaluator Machine Tests"
    [ testCase "Simple Operations" $ do
        let int = flip SInteger SEmpty
            home = DLocation
        assertEqual
          "Working Push Int 3s."
          ( ok
              ( mempty @MachineMemory
                  & stacks .~ M.fromList [(home, mempty & stack .~ S.fromList [int 3, int 3])]
              )
          )
          (evaluate' . mconcat $ [int 3, int 3])

        let char = SChar 'a' SEmpty

        assertEqual
          "Working Push Chars."
          ( ok
              ( mempty @MachineMemory
                  & stacks .~ M.fromList [(home, mempty & stack .~ S.fromList [char, char])]
              )
          )
          (evaluate' . mconcat $ [char, char])
        let pop var loc = SPop (Variable var) loc SEmpty
            varT var = SVariable (Variable var) SEmpty
            var = Variable
            popX = pop "x" home
            popY = pop "y" home
            varX = varT "x"
            varY = varT "y"

        assertEqual
          "Working Pop 1 term."
          ( ok
              ( mempty @MachineMemory
                  & binds . at (var "x") ?~ char
              )
          )
          (evaluate' . mconcat $ [char, popX])

        assertEqual
          "Working Pop 1 term and put it back."
          ( ok
              ( mempty @MachineMemory
                  & binds . at (var "x") ?~ char
                  & stacks . at home ?~ review stack (S.fromList [char])
              )
          )
          (evaluate' . mconcat $ [char, popX, varX])

        assertEqual
          "Pop 1 term and bind it to another variable."
          ( ok
              ( mempty @MachineMemory
                  & binds . at (var "x") ?~ char
                  & binds . at (var "y") ?~ char
                  & stacks . at home ?~ review stack (S.fromList [char])
              )
          )
          (evaluate' . mconcat $ [char, popX, varX, popY, varY])

        assertEqual
          "Pop 1 term from stack of 2."
          ( ok
              ( mempty @MachineMemory
                  & binds . at (var "x") ?~ char
                  & stacks . at home ?~ review stack (S.fromList [char])
              )
          )
          (evaluate' . mconcat $ [char, char, popX])

        assertEqual
          "Working Pop 1 term from stack of 0."
          ( ok
              ( mempty @MachineMemory
                  & binds . at (var "x") ?~ SEmpty
              )
          )
          (evaluate' . mconcat $ [popX])

        let op x = SVariable (Variable x) SEmpty
        assertEqual
          "Addition."
          ( ok
              ( mempty @MachineMemory
                  & stacks .~ M.fromList [(home, mempty & stack .~ S.fromList [int 6])]
              )
          )
          (evaluate' . mconcat $ [int 3, int 3, op "+"])

        assertEqual
          "Division."
          ( ok
              ( mempty @MachineMemory
                  & stacks .~ M.fromList [(home, mempty & stack .~ S.fromList [int 1])]
              )
          )
          (evaluate' . mconcat $ [int 3, int 3, op "/"])

        assertEqual
          "Working error."
          (err $ TypeMissmatch "Expected 2 Ints, got: SChar 'a' SEmpty SChar 'a' SEmpty")
          (evaluate' . mconcat $ [char, char, op "/"])
    ]
