{-# LANGUAGE OverloadedStrings #-}
module Test.Space.Language (test) where

import Control.Lens
import Data.Map qualified as M
import Data.Sequence qualified as S
import Data.String
import Data.Text qualified as T
import Prettyprinter (pretty, Pretty)
import Space
import Space.Interface.REPL
import Space.Language qualified as L
import Space.Parser.Term as X
import Space.Parser.Token as X
import Test.Tasty
import Test.Tasty.HUnit hiding (assert)

--  x           : { a -> b }
-- [x]          : { {} -> ({a->b})@ }
-- [x];<x>      :
-- <x:{a -> b}> : { ({a -> b})@ -> {} }

test =
  testGroup
    "Language Unit Tests."
    [ testGroup
        "Pretty Show Terms."
        [ testCase "Empty." $ sPretty SEmpty @?= "*"
        , testCase "Variable." $ sPretty (SVariable "x" SEmpty)  @?= "x"
        , testCase "Integer." $ sPretty (SInteger 3 SEmpty) @?= "3"
        , testCase "Char." $ sPretty (SChar 's' SEmpty) @?= "'s'"
        , testCase "Push default location." $ sPretty (SPush (SChar 's' SEmpty) DLocation SEmpty) @?= "['s'];"
        , testCase "Push arbitrary location." $ sPretty (SPush (SChar 's' SEmpty) "Lo" SEmpty) @?= "['s']@Lo;"
        , testCase "Pop default location." $ sPretty (SPop "x" DLocation SEmpty) @?= "<x>;"
        , testCase "Pop arbitrary location." $ sPretty (SPop "x" "Lo" SEmpty) @?= "<x>@Lo;"
        , testCase "Pop typed default location." $ sPretty (SPopT "x" DLocation TEmpty SEmpty) @?= "<x:{}>;"
        , testCase "Pop typed arbitrary location." $ sPretty (SPopT "x" "Lo" TEmpty SEmpty) @?= "<x:{}>@Lo;"
        ]
    , testGroup
      "Pretty Show Types."
      [ testCase "Type Empty." $ sPretty TEmpty @?= "{}"
      , testCase "Type Variable." $ sPretty (TVariable "x" TEmpty) @?= "x"
      , testCase "Type Integer." $ sPretty (TConstant TInt TEmpty) @?= "Z"
      , testCase "Type Char." $ sPretty (TConstant TChar TEmpty) @?= "Ch"
      , testCase "Type 5 Char." $ sPretty (TMany 5 (TConstant TChar TEmpty) TEmpty) @?= "5.{Ch}"
      , testCase "Type Arrow." $ sPretty (TArrow TEmpty TEmpty TEmpty) @?= "[{} -> {}]"
      , testCase "Type 5 Arrow." $ sPretty (TMany 5 (TArrow TEmpty TEmpty TEmpty) TEmpty) @?= "5.{[{} -> {}]}"
      , testCase "Type Default Location Empty." $ sPretty (TLocation DLocation TEmpty TEmpty)  @?= "({})@"
      , testCase "Type Arbitrary Location Empty." $ sPretty (TLocation (Location "IO") TEmpty TEmpty)  @?= "({})@IO"
      , let
          termR = "[(x)@Io;Z;(Ch)@ -> ({})@;5.{Ch}]"
          term =
             ( TLocation "Io" (TVariable "x" TEmpty) $
               TConstant TInt $
               TLocation DLocation (TConstant TChar TEmpty) $
               TEmpty
             ) ->:
             ( TLocation DLocation TEmpty $
               TMany 5 (TConstant TChar TEmpty) $
               TEmpty
             )
        in
          testCase "Type Vector with all types of Types." $ sPretty term @?= termR
      ]
    ]
 where
  sPretty :: Pretty a => a -> String
  sPretty = show . pretty
