module Main where

import Space.Evaluator
import Space.Syntax.Parser ( parseDefinitions )
import Prettyprinter
import Options.Applicative

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  opts <- execParser (info sPCOptions fullDesc)
  content <- readFile (_file opts)
  case parseDefinitions content of
    Left e -> error $ show e 
    Right (term,_) ->   do
      res <- evalTerm term
      case res of 
        Right res ->  (print . pretty) res
        Left e -> print e

newtype SPCOptions =  SPCOptions { _file :: String }

sPCOptions :: Parser SPCOptions
sPCOptions
  = SPCOptions
  <$> argument str (metavar "Input File")
