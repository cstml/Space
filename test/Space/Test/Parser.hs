module Space.Test.Parser (tParser) where

import Examples.ExampleTerms 
import Test.Tasty
import Space.Syntax.Parser
import Space.Syntax.Types
import Test.Tasty.HUnit

tParser :: TestTree
tParser = testGroup "Parser Tests"
  [ t1 "x" (Right (Variable "x" NoOp))
  , t1 "x y { x }"  $ Right
                    $ Variable "x"
                    $ Variable "y"
                    $ Closure (Variable "x" NoOp)
                    $ NoOp
  , let file = "test/Examples/example-terms.sp" in testCase file $ do 
      input <- readFile file 
      parseExpressions input @?= Right exampleTerm1
  ]
  where
    t1 s r =  testCase s $ parseTerm s @?= r
