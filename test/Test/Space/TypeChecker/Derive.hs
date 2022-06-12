module Test.Space.TypeChecker.Derive (test) where

import Space
import Test.Tasty
import Test.Tasty.HUnit

-- FIXME
test =
  testGroup
    "Derivation Tests."
    [ testCase "REFL" $ 1 @=? 1
    , testCase "Simple Derivation" $
        unDerive (derive SEmpty)
          @?= Right (DEmpty (TJudgement {_jContext = TContext {_tContext = []}, _jTerm = SEmpty, _jType = TEmpty}))
          {-, testCase "Trivial term type infer." $
              unDerive (derive $ SInteger 1 $ SInteger 2 $ SInteger 3 SEmpty)
                @?= let commonContext =
                          [
                            ( SEmpty
                            , TEmpty ->: TEmpty
                            )
                          ,
                            ( SInteger 3 SEmpty
                            , TEmpty ->: TConstant TInt TEmpty
                            )
                          ,
                            ( SInteger 1 SEmpty
                            , TEmpty ->: TConstant TInt TEmpty
                            )
                          ,
                            ( SInteger 1 SEmpty
                            , TEmpty ->: TConstant TInt TEmpty
                            )
                          ]
                     in Right
                          ( DSeq
                              ( TJudgement
                                  { _jContext = TContext commonContext
                                  , _jTerm = SInteger 1 (SInteger 2 (SInteger 3 SEmpty))
                                  , _jType = TVariable (TVariableAtom "a1") TEmpty
                                  }
                              )
                              ( DSeq
                                  ( TJudgement
                                      { _jContext = TContext commonContext
                                      , _jTerm = SInteger 2 (SInteger 3 SEmpty)
                                      , _jType = TVariable (TVariableAtom "a2") TEmpty
                                      }
                                  )
                                  ( DSeq
                                      ( TJudgement
                                          { _jContext = TContext commonContext
                                          , _jTerm = SInteger 3 SEmpty
                                          , _jType = TVariable (TVariableAtom "a3") TEmpty
                                          }
                                      )
                                      ( DEmpty
                                          ( TJudgement
                                              { _jContext = TContext commonContext
                                              , _jTerm = SEmpty
                                              , _jType = TEmpty
                                              }
                                          )
                                      )
                                  )
                              )
                          )-}
    ]
