module Test.Space.Evaluator.Machine where

import Control.Lens
import Data.Map qualified as M
import Data.Sequence qualified as S
import Space
import Space.Evaluator.Machine
import Test.Tasty
import Test.Tasty.HUnit

test1 =
  testGroup
    "Evaluator Machine Tests"
    [ testCase "Simple Operations" $ do
        let int = flip SInteger SEmpty
            home = Location "Ho"
        assertEqual
          "Working Push Int 3s."
          ( mempty @MachineMemory
              & stacks .~ M.fromList [(home, mempty & stack .~ S.fromList [int 3, int 3])]
          )
          (evaluate' . mconcat $ [int 3, int 3])

        let char = SChar 'a' SEmpty

        assertEqual
          "Working Push Chars."
          ( mempty @MachineMemory
              & stacks .~ M.fromList [(home, mempty & stack .~ S.fromList [char, char])]
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
          ( mempty @MachineMemory
              & binds . at (var "x") ?~ char
          )
          (evaluate' . mconcat $ [char, popX])

        assertEqual
          "Working Pop 1 term and put it back."
          ( mempty @MachineMemory
              & binds . at (var "x") ?~ char
              & stacks . at home ?~ review stack (S.fromList [char])
          )
          (evaluate' . mconcat $ [char, popX, varX])

        assertEqual
          "Pop 1 term and bind it to another variable."
          ( mempty @MachineMemory
              & binds . at (var "x") ?~ char
              & binds . at (var "y") ?~ char
              & stacks . at home ?~ review stack (S.fromList [char])
          )
          (evaluate' . mconcat $ [char, popX, varX, popY, varY])

        assertEqual
          "Pop 1 term from stack of 2."
          ( mempty @MachineMemory
              & binds . at (var "x") ?~ char
              & stacks . at home ?~ review stack (S.fromList [char])
          )
          (evaluate' . mconcat $ [char, char, popX])

        assertEqual
          "Working Pop 1 term from stack of 0."
          ( mempty @MachineMemory
              & binds . at (var "x") ?~ SEmpty
          )
          (evaluate' . mconcat $ [popX])

        let op x = SVariable (Variable x) SEmpty
        assertEqual
          "Working Addition."
          ( mempty @MachineMemory
              & stacks .~ M.fromList [(home, mempty & stack .~ S.fromList [int 6])]
          )
          (evaluate' . mconcat $ [int 3, int 3, op "+"])

        assertEqual
          "Working division."
          ( mempty @MachineMemory
              & stacks .~ M.fromList [(home, mempty & stack .~ S.fromList [int 1])]
          )
          (evaluate' . mconcat $ [int 3, int 3, op "/"])
    ]
