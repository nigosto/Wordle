module Main where

import System.IO
  ( BufferMode (BlockBuffering),
    hSetBuffering,
    stdout,
  )
import Modes.Modes (chooseGameMode)

main :: IO ()
main = do
  hSetBuffering stdout $ BlockBuffering $ Just 1
  filename <- putStr "Enter wordlist's filename: " >> getLine
  contents <- readFile filename
  mode <- putStr "Choose game mode (game, helper): " >> getLine
  chooseGameMode contents mode main
