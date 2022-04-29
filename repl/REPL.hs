module Main where

import Control.Lens
import Control.Monad
import Prettyprinter
import Space
import Space.Interface.REPL

data SpaceiConfig = SpaceiConfig
  { _siWelcome :: String
  , _siPrompt :: String
  , _siBye :: String
  , _siHelp :: String
  }

makeLenses ''SpaceiConfig

siVersion = "v0.0.0 "

spaceiStdConfig =
  SpaceiConfig
    { _siWelcome =
        mconcat $
          (<> "\n")
            <$> [ "Space.i, version: " ++ siVersion
                , ":h for help prompt."
                , "Happy Hacking!"
                ]
    , _siPrompt = "γ> "
    , _siBye = "See you later!\n"
    , _siHelp =
        mconcat $
          (<> "\n")
            <$> [":l to load a file", ":q to quit", ":h for help"]
    }

main :: IO ()
main = do
  putStrLn (spaceiStdConfig ^. siWelcome)
  dispatch mempty

step :: MachineMemory -> String -> IO MachineMemory
step mem input = do
  let (out, mem') = replEval input mem
  print (pretty out)
  pure mem'

data Command = CInterpret | CQuit | CLoad | CHelp

readCommand :: String -> (Command, String)
readCommand = \case
  (':' : c : ss) ->
    case c of
      'q' -> (,) CQuit mempty
      'l' -> (,) CLoad ss
      'h' -> (,) CHelp mempty
      _ -> (,) CHelp mempty
  x -> (CInterpret, x)

dispatch :: MachineMemory -> IO ()
dispatch mem = do
  (comm, input) <- readCommand <$> replRead
  exeCommand comm mem input

exeCommand :: Command -> MachineMemory -> String -> IO ()
exeCommand = \case
  CQuit -> \ _ _ -> pure ()
  CInterpret -> \ m s -> step m s >>= dispatch
  CLoad -> \ m (_ : path) -> readFile path >>= step m >>= dispatch
  cHelp -> \m _ -> putStrLn (spaceiStdConfig ^. siHelp) >> dispatch m
