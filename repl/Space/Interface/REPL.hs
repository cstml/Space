module Space.Interface.REPL where

import Control.Lens
import Space

replRead = readLn @String

replEval :: String -> IO (String, SMachine)
replEval s = do
  let pr = parseTerm s
  case pr of
    Left e -> error . show $ e
    Right t -> evaluate t
  
