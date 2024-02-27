module Modes.Guesser.Normal where

import Modes.Guesser.State (inputColors, GameState, toGuess, updateGameState, chooseStartingWord, findBestWord)
import Colors.Common (Color(Green))
import Modes.Common (color, outputWinMessage)
import System.Random (newStdGen)

playNormalTurn :: GameState -> IO () -> String -> IO ()
playNormalTurn state start word = do
  guess <- toGuess word 0 <$> (putStrLn word >> inputColors) 
  if all ((== Green) . color) guess
  then outputWinMessage "Hooray! I found the word!" start
  else chooseWordNormal (updateGameState state guess word) start

chooseWordNormal :: GameState -> IO () -> IO ()
chooseWordNormal state@([], words) start = newStdGen >>= playNormalTurn state start . chooseStartingWord words
chooseWordNormal (_, []) _ = putStrLn "Your word is not part of the word list! Ending current session!"
chooseWordNormal state start = playNormalTurn state start $ findBestWord state