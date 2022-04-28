module Space.Interface.REPL where

import Control.Lens
import Space

replRead = getLine

replEval :: String -> MachineMemory -> (String, MachineMemory)
replEval s mem =
  let
    pr = parseTerm s
  in
    case pr of
      Left e -> (show $ e, mem)
      Right term -> case eval mem term of
        Left e -> (show e, mem)
        Right mem' -> (show mem', mem')
      
