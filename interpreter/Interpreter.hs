module Main where

import Control.Lens
import Control.Monad
import Options.Applicative qualified as OPT
import Prettyprinter
import Space
import Space.Aux.Evaluate
import Space.Evaluator.Implementation.IO
import Space.Evaluator.Implementation.Pure

data Input
  = FileInput String
  | StdInput
  | Command String

data Debug = Production | Debug

data Interpreter = Interpreter Input Debug

main :: IO ()
main = do
  (Interpreter inp dbg) <- OPT.execParser (OPT.info inputFrom OPT.fullDesc)
  rt <- case inp of
    FileInput x -> readFile x
    StdInput -> getLine
    Command x -> pure x
  let t = either (error . show) id . parseTerm $ rt
  (m :: MachineMemory) <- either (error . show @MException) id <$> eval mempty t
  case dbg of
    Debug -> print $ pretty m
    _ -> pure ()

command :: OPT.Parser Input
command =
  Command
    <$> OPT.strOption
      ( OPT.long "command"
          <> OPT.short 'c'
          <> OPT.metavar "Command"
          <> OPT.help "Command for executing"
      )

fileInput :: OPT.Parser Input
fileInput =
  FileInput
    <$> OPT.strOption
      ( OPT.long "file"
          <> OPT.short 'f'
          <> OPT.metavar "FILENAME"
          <> OPT.help "Input file"
      )

stdInput :: OPT.Parser Input
stdInput =
  OPT.flag'
    StdInput
    ( OPT.long "stdin"
        <> OPT.help "Read from stdin"
    )

inputFrom :: OPT.Parser Interpreter
inputFrom = Interpreter <$> (fileInput OPT.<|> stdInput OPT.<|> command) <*> oDebug

oDebug :: OPT.Parser Debug
oDebug =
  OPT.flag
    Production
    Debug
    ( OPT.long "debug"
        <> OPT.short 'd'
        <> OPT.help "turn debugging on"
    )
