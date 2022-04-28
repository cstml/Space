module Main where

import Control.Lens
import Space
import Space.Interface.REPL
import Control.Monad

main :: IO ()
main = do
    mem <- initialise
    void $ loop mem
    
initialise :: IO MachineMemory
initialise = do
    input <- replRead
    let (out, mem') = replEval input mempty
    print out
    return mem'

loop :: MachineMemory -> IO MachineMemory
loop mem = do
  input <- replRead
  let (out, mem') = replEval input mem
  print out
  loop mem'
  
