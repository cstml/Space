module Main where

import Brick
import Brick.Main qualified as M
import Brick.Widgets.Edit qualified as E
import Control.Lens
import Space.Interface.REPL

main :: IO ()
main = do
  st <- M.defaultMain repl initialState
  putStrLn "In input 1 you entered:\n"
  putStrLn $ unlines $ E.getEditContents $ st ^. editor
  putStrLn "In input 2 you entered:\n"
  putStrLn $ unlines $ E.getEditContents $ st ^. editor
  st <- M.defaultMain repl st
  putStrLn "In input 1 you entered:\n"
  putStrLn $ unlines $ E.getEditContents $ st ^. editor
  putStrLn "In input 2 you entered:\n"
  putStrLn $ unlines $ E.getEditContents $ st ^. editor
