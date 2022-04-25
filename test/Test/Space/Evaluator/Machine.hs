module Test.Space.Evaluator.Machine where

import Space
import Space.Language
import Space.Evaluator
import Test.Tasty
import Test.Tasty.HUnit
import Control.Lens
import qualified Data.Sequence as S
import qualified Data.Map as M

test1 =
  testGroup
    "Evaluator Machin Tests"
    [ testCase "Operations" $ do
        let int = (SInteger 3)        
        assertEqual "Working Push Ints."
          (evaluate' [int,int]) 
          ( mempty @MachineMemory
            & stacks .~ M.fromList [(Location "Ho", mempty & stack .~ S.fromList [int,int])]
          )
          
        let char = (SChar 'a') 
        assertEqual "Working Push Chars." 
          (evaluate' [char,char]) 
          ( mempty @MachineMemory
            & stacks .~ M.fromList [(Location "Ho", mempty & stack .~ S.fromList [char,char])]
          )      
    ]
