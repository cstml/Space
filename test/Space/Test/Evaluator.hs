{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-} 
module Space.Test.Evaluator (tEvaluator) where

import Space.Evaluator
import Test.Tasty
import Space.Syntax.Parser
import Space.Syntax.Types
import Test.Tasty.HUnit
import qualified Data.Map as M

tEvaluator :: TestTree
tEvaluator = testGroup "Evaluator Tests"
  [ tNoOp
  , tPushVar
  , tBindVar
  , tBindInClosure
  , tEvalInClosure
  , tIf
  , tIf2
  ]


noOp = NoOp
one = Variable "1"
pushS = Push Spine 

tNoOp :: TestTree
tNoOp = testCase " " $ do
  res <- runEval $ noOp
  res @?= (NoOp, (mempty, mempty))

tPushVar :: TestTree
tPushVar = testCase "[1]@_" $ do
  res <- runEval $ pushS (one noOp) noOp
  res @?= (NoOp, (mempty, stacks))
  where
    stacks = MkStacks $
      M.fromList [(Spine,[Variable "1" NoOp])]

tBindVar :: TestTree
tBindVar = testCase "Bind 1" $ do
  res <- runEval $ Push Spine (Variable "1" NoOp) $ Bind Spine "x" $ NoOp
  res @?= (NoOp, (binds, stacks))
  where
    stacks = MkStacks $ M.fromList [(Spine,[])]
    binds = MkBindings $ M.fromList [("x", Variable "1" NoOp)]

tBindInClosure :: TestTree
tBindInClosure = testCase "Bind 1 Variable in closure" $ do
  res <- runEval $ Push Spine (Variable "1" NoOp) $ Closure (Bind Spine "x" NoOp) $ NoOp
  res @?= (NoOp, (binds, stacks))
  where
    stacks = MkStacks $ M.fromList [(Spine,[])]
    binds = MkBindings $ M.fromList []

tEvalInClosure :: TestTree
tEvalInClosure = testCase "Bind + Eval 1 Variable in closure" $ do
  res <- runEval $ Push Spine (Variable "1" NoOp) $ Closure (Bind Spine "x" $ Variable "x" $ NoOp) $ NoOp
  res @?= (NoOp, (binds, stacks))
  where
    stacks = MkStacks $ M.fromList [(Spine,[Variable "1" NoOp])]
    binds = MkBindings $ M.fromList []
    
tIf :: TestTree
tIf = testCase "eval if" $ do
  res <- runEval $ Push Return (Variable "2" NoOp)
                 $ Push Return (Variable "1" NoOp)
                 $ Push Return (Variable "true" NoOp)
                 $ Variable "if"
                 $ NoOp
  res @?= (NoOp, (binds, stacks))
  where
    stacks = MkStacks $ M.fromList [(Spine,[Variable "1" NoOp])
                                   ,(Return,[])]
    binds = MkBindings $ M.fromList []

tIf2 :: TestTree
tIf2 = testCase "eval if" $ do
  res <- runEval $ Push Return (Variable "2" NoOp)
                 $ Push Return (Variable "1" NoOp)
                 $ Push Return (Variable "false" NoOp)
                 $ Variable "if"
                 $ NoOp
  res @?= (NoOp, (binds, stacks))
  where
    stacks = MkStacks $ M.fromList [(Spine,[Variable "2" NoOp])
                                   ,(Return,[])]
    binds = MkBindings $ M.fromList []
