module Main where

import Control.Lens
import Control.Monad
import Prettyprinter
import Space
import Space.Interface.REPL

main :: IO ()
main = do
  putStrLn (spaceiStdConfig ^. siWelcome)
  dispatch mempty
