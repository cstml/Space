module Test.Space.Parser where

import Control.Lens
import Data.Map qualified as M
import Data.Sequence qualified as S
import Data.String
import Data.Text qualified as T
import Data.Void
import Space
import Space.Language qualified as L
import Space.Parser.Term as X
import Space.Parser.Token as X
import Test.Tasty
import Test.Tasty.HUnit
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Text.Megaparsec.Char.Lexer qualified as P

ok :: forall a b. a -> Either b a
ok = pure

tParseTerm :: String -> Either (P.ParseErrorBundle T.Text Void) L.Term
tParseTerm = P.parse pTerm "Test Input" . fromString

unit =
  let stdTest r1 t1 r2 t2 = do
        let str = r1 <> ";*"
        assertEqual str (Right $ t1 SEmpty) (tParseTerm str)

        let str = r1
        assertEqual str (Right $ t1 SEmpty) (tParseTerm str)

        let str = r1 <> ";"
        assertEqual str (Right $ t1 SEmpty) (tParseTerm str)

        let str = r1 <> "    ;    *"
        assertEqual str (Right $ t1 SEmpty) (tParseTerm str)

        let str = r1 <> " ; " <> r2 <> " ; *"
        assertEqual str (Right $ t1 $ t2 SEmpty) (tParseTerm str)

        let str = r1 <> " ; " <> r2 <> ";"
        assertEqual str (Right $ t1 $ t2 SEmpty) (tParseTerm str)

        let str = r1 <> " {-; " <> r2 <> " -}; *"
        assertEqual str (Right $ t1 SEmpty) (tParseTerm str)
   in testGroup
        "Parser Unit Tests"
        [ testCase "Parse empty Term" $ assertEqual "*" (Right SEmpty) (tParseTerm "*")
        , testCase "Parse Push empty." $
            let p = "[*];*"
             in assertEqual p (Right $ SPush SEmpty DLocation SEmpty) (tParseTerm p)
        , testCase "Parse Push Variable." $
            let p = "[x;*];*"
             in assertEqual p (Right $ SPush (SVariable (Variable "x") SEmpty) DLocation SEmpty) (tParseTerm p)
        , testCase "Parse Push Variable." $
            stdTest
              "[x]"
              (SPush (SVariable (Variable "x") SEmpty) DLocation)
              "y"
              (SVariable (Variable "y"))
        , testCase "Parse Char." $ stdTest "'x'" (SChar 'x') "'y'" (SChar 'y')
        , testCase "Parse Int." $ stdTest "1" (SInteger 1) "20" (SInteger 20)
        , let t x = SPush (SVariable (Variable x) SEmpty) DLocation
           in testCase "Parse Push Default Location." $ stdTest "[ x ; * ]" (t "x") "[y;*]" (t "y")
        , let t x = SPush (SVariable (Variable x) SEmpty) DLocation
           in testCase "Parse Push Default Location." $ stdTest "[ x ]" (t "x") "[ y ]" (t "y")
        , let t x = SPush (SVariable (Variable x) SEmpty) DLocation
           in testCase "Parse Push Default Locataion." $ stdTest "[ x;* ]" (t "x") "[y;*]" (t "y")
        , let t x = SPush (SVariable (Variable x) SEmpty) DLocation
           in testCase "Parse Push Default Locataion." $ stdTest "[x;*]" (t "x") "[ y;*   ]" (t "y")
        , let t x = SPush (SInteger x SEmpty) DLocation
           in testCase "Parse Push Default Locataion." $ stdTest "[1;*]" (t 1) "[ 2;*   ]" (t 2)
        , let t x l = SPush (SVariable (Variable x) SEmpty) (Location l)
           in testCase "Parse Push arbitrary." $ stdTest "@In[ x ; * ]" (t "x" "In") "@Ou[y;*]" (t "y" "Ou")
        , let t x l = SPush (SVariable (Variable x) SEmpty) (Location l)
           in testCase "Parse Push arbitrary." $ stdTest "@In[ x;* ]" (t "x" "In") "@Ou[y;*]" (t "y" "Ou")
        , let t x = SPop (Variable x) DLocation
           in testCase "Parse Pop Ho." $ stdTest "<x>" (t "x") "<y>" (t "y")
        , let t x l = SPop (Variable x) (Location l)
           in testCase "Parse Pop arbitrary." $ stdTest "@In<x>" (t "x" "In") "@Yo<y>" (t "y" "Yo")
        , testCase "Parse Equal (Variable)." $
            stdTest "==" (SVariable (Variable "==")) "/=" (SVariable (Variable "/="))
        ]
