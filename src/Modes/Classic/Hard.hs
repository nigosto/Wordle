module Modes.Classic.Hard where

import Modes.Common (Guess, color)
import Modes.Classic.State (wordExists, playTurn, GameState, secretWord)
import Utils (generateRandomNumber)
import System.Random (newStdGen)
import Colors.Colors (findWrongColors, findColors)

askForWordHard :: GameState -> [Guess] -> Bool -> IO ()
askForWordHard state previousGuesses hasLied = 
  playTurn state nextTurn action
    where nextTurn = askForWordHard state previousGuesses hasLied
          action guess = do
            willLie <- (not hasLied &&) . (20 >=) . generateRandomNumber 0 100 <$> newStdGen
            if willLie
            then newStdGen >>= print . map color . findWrongColors (secretWord state) guess previousGuesses >>
                 askForWordHard state previousGuesses True
            else let newGuess = findColors (secretWord state) guess 
                 in print (map color newGuess) >> askForWordHard state (newGuess : previousGuesses) hasLied