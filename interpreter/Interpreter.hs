module Main where

import Control.Lens
import Control.Monad
import Options.Applicative qualified as OPT
import Prettyprinter
import Space
import Space.Evaluator.Implementation.IO qualified as II

data Input
  = FileInput String
  | StdInput
  | Command String

main :: IO ()
main = do
  inp <- OPT.execParser (OPT.info inputFrom OPT.fullDesc)
  rt <- case inp of
    FileInput x -> readFile x
    StdInput -> getLine
    Command x -> pure x
  let t = either (error . show) id . parseTerm $ rt
  m <- either (error . show) id <$> II.eval mempty t
  print m

-- print "()"

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

inputFrom :: OPT.Parser Input
inputFrom = fileInput OPT.<|> stdInput OPT.<|> command
