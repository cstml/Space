module Main where

import Brick
import Brick.Main qualified as M
import Brick.Widgets.Edit qualified as E
import Control.Lens
import Space
import Space.Interface.REPL

main :: IO ()
main = do
  st <- M.defaultMain repl initialState
  putStrLn "In input 1 you entered:\n"
  putStrLn $ unlines $ E.getEditContents $ st ^. editor
  putStrLn "In input 2 you entered:\n"
  putStrLn $ unlines $ E.getEditContents $ st ^. editor
  (show . parseTerm -> nStr) <- pure $ mconcat $ E.getEditContents $ st ^. editor
  let nst = st & editor .~ nEd nStr
  st <- M.defaultMain repl nst
  putStrLn "In input 1 you entered:\n"
  putStrLn $ unlines $ E.getEditContents $ st ^. editor
  putStrLn "In input 2 you entered:\n"
  putStrLn $ unlines $ E.getEditContents $ st ^. editor
