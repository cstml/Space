module Space.Interface.Spci where

import Control.Lens
import Prettyprinter
import Space
import Space.Aux.Evaluate
import Space.Evaluator.Implementation.Pure

data SpaceiConfig = SpaceiConfig
  { _siWelcome :: String
  , _siPrompt :: String
  , _siBye :: String
  , _siHelp :: String
  }

makeLenses ''SpaceiConfig

siVersion = "v0.0.1 "

spaceiStdConfig =
  SpaceiConfig
    { _siWelcome =
        mconcat $
          (<> "\n")
            <$> [ "Space.i, version: " <> siVersion
                , ":h for help prompt."
                , ""
                , "Run with rlwrap for easy use."
                , ""
                , "Happy Hacking!"
                ]
    , _siPrompt = "γ> "
    , _siBye = "See you later!\n"
    , _siHelp =
        mconcat $
          (<> "\n")
            <$> [":l to load a file."
                , ":q to quit."
                , ":h for help."
                , ":r to reset machine memory."
                ]
    }

replEval :: String -> MachineMemory -> (String, MachineMemory)
replEval s mem =
  let pr = parseTerm s
   in case pr of
        Left e -> (show e, mem)
        Right term -> case eval mem term of
          Identity (Left (e :: MException)) -> (show . pretty $ e, mem)
          Identity (Right mem') -> (show . pretty $ mem', mem')

step :: MachineMemory -> String -> IO MachineMemory
step mem input = do
  let (out, mem') = replEval input mem
  print (pretty out)
  pure mem'

data Command = CInterpret | CQuit | CLoad | CHelp | CReset

readCommand :: String -> (Command, String)
readCommand = \case
  (':' : c : ss) ->
    case c of
      'q' -> (,) CQuit mempty
      'l' -> (,) CLoad ss
      'h' -> (,) CHelp mempty
      'r' -> (,) CReset mempty
      _ -> (,) CHelp mempty
  x -> (CInterpret, x)

dispatch :: MachineMemory -> IO ()
dispatch mem = do
  (comm, input) <- readCommand <$> getLine
  exeCommand comm mem input

exeCommand :: Command -> MachineMemory -> String -> IO ()
exeCommand = \case
  CQuit -> \_ _ -> putStrLn (spaceiStdConfig ^. siBye)
  CInterpret -> \m s -> step m s >>= dispatch
  CLoad -> \m (_ : path) -> readFile path >>= step m >>= dispatch
  CReset -> const . const $ putStrLn "Machine Memory reset." >> dispatch mempty
  cHelp -> \m _ -> putStrLn (spaceiStdConfig ^. siHelp) >> dispatch m
