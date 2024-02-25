module Modes.Game.Hard where

import Modes.Common (Guess, color)
import Modes.Game.State (wordExists, playTurn)
import Utils (generateRandomNumber)
import System.Random (newStdGen)
import Colors.Colors (findWrongColors, findColors)

askForWordHard :: String -> [String] -> IO () -> [Guess] -> Bool -> IO ()
askForWordHard secretWord words start previousGuesses hasLied = 
  playTurn secretWord words start nextTurn action
    where nextTurn = askForWordHard secretWord words start previousGuesses hasLied
          action guess = do
            willLie <- (not hasLied &&) . (20 >=) . generateRandomNumber 0 100 <$> newStdGen
            if willLie
            then newStdGen >>= print . map color . findWrongColors secretWord guess previousGuesses >>
                 askForWordHard secretWord words start previousGuesses True
            else print (map color $ findColors secretWord guess) >>
                 askForWordHard secretWord words start (findColors secretWord guess : previousGuesses) hasLied