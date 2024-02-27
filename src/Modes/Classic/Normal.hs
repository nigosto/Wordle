module Modes.Classic.Normal where

import Modes.Classic.State (wordExists, playTurn, wordExists, GameState, secretWord)
import Modes.Common (color)
import Colors.Colors (findColors)

askForWordNormal :: GameState -> IO ()
askForWordNormal state =
  playTurn state nextTurn action
    where nextTurn = askForWordNormal state
          action guess = print (map color $ findColors (secretWord state) guess) >> nextTurn
