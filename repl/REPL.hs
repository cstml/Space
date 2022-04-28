module Main where

import Control.Lens
import Space
import Space.Interface.REPL
import Control.Monad
import Prettyprinter

data SpaceiConfig = SpaceiConfig
  { _siWelcome :: String
  , _siPrompt :: String
  , _siBye :: String
  }

makeLenses ''SpaceiConfig

siVersion = "v0.0.0 "

spaceiStdConfig = SpaceiConfig
 { _siWelcome = "Space.i, version: " ++ siVersion ++ ".\nHappy Hacking!"
 , _siPrompt = "γ> "
 , _siBye = "See you later!\n"
 }

main :: IO ()
main = do
    putStrLn (spaceiStdConfig ^. siWelcome)
    dispatch mempty

step :: MachineMemory -> String -> IO MachineMemory
step mem input = do
  let (out, mem') = replEval input mem
  putStrLn $ show $ pretty out
  pure mem'

data Command = CInterpret | CQuit | CLoad 

readCommand :: String -> (Command,String)
readCommand = \case
  (':':c: ss) ->
    case c of
      'q' -> (CQuit,mempty)
      'l' -> (CLoad,ss)          
  x -> (CInterpret,x)

dispatch :: MachineMemory -> IO ()
dispatch mem = do  
  (comm,input) <- readCommand <$> replRead
  exeCommand comm mem  input

exeCommand :: Command -> MachineMemory -> String -> IO ()
exeCommand = \case
  CQuit -> \ _ -> \_ -> pure ()
  CInterpret -> \m -> \s -> step m s >>= dispatch
  CLoad -> \m -> \(_:path) -> readFile path >>= step m >>= dispatch
    
