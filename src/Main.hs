module Main where

import System.IO
  ( BufferMode (BlockBuffering),
    hSetBuffering,
    stdout,
  )
import Modes.Modes (chooseGameMode)

main :: IO ()
main = do
  hSetBuffering stdout (BlockBuffering $ Just 1)
  contents <- readFile "assets/wordlist.txt"
  putStr "Choose game mode (game, helper): "
  mode <- getLine
  chooseGameMode contents mode main
