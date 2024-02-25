module Modes.Game.Normal where

import Modes.Game.State (wordExists, playTurn, wordExists)
import Modes.Common (color)
import Colors.Colors (findColors)

askForWordNormal :: String -> [String] -> IO () -> IO ()
askForWordNormal secretWord words start =
  playTurn secretWord words start nextTurn action
    where nextTurn = askForWordNormal secretWord words start
          action guess = print (map color $ findColors secretWord guess) >> nextTurn
