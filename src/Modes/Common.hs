module Modes.Common where

import System.Exit (exitSuccess)

type LetterInfo = (Char, String, Int)
type Guess = [LetterInfo]

letter :: LetterInfo -> Char
letter (l, _, _) = l

color :: LetterInfo -> String
color (_, c, _) = c

position :: LetterInfo -> Int
position (_, _, p) = p

outputWinMessage :: String -> IO () -> IO ()
outputWinMessage message start = do
  putStrLn message >> putStr "Would you like to play again (y/n): "
  answer <- getLine 
  if answer == "y" || answer == "yes" then start else exitSuccess
