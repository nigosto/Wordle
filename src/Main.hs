module Main where

import System.IO
  ( BufferMode (BlockBuffering),
    hSetBuffering,
    stdout,
  )
import Modes.Modes (chooseGameMode, enterGameMode)

main :: IO ()
main = do
  hSetBuffering stdout $ BlockBuffering $ Just 1
  contents <- putStr "Enter wordlist's filename: " >> getLine >>= readFile
  mode <- enterGameMode
  chooseGameMode contents mode main
