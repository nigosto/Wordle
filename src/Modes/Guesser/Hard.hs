module Modes.Guesser.Hard where

import Modes.Common (Guess, color, outputWinMessage)
import Modes.Guesser.State
    ( statesGuesses,
      statesWords,
      updateGameState,
      updateWordList,
      toGuess, inputColors, chooseStartingWord, findBestWord, sumRemovedWords, GameState )
import Colors.Common (Color(Green))
import System.Random (newStdGen)
import Data.List (maximumBy)

updateComplexGameState :: [GameState] -> Guess -> String -> [GameState]
updateComplexGameState states guess word =
  let (previousGuesses : restGuesses) = statesGuesses states
      (previousWords : restWords) = statesWords states
      previousState = (previousGuesses, previousWords)
      (newGuesses, newWords) = updateGameState previousState guess word
      updatedRestGuesses = map (guess:) restGuesses
      restStates = zip updatedRestGuesses restWords
  in zip (newGuesses : previousGuesses : updatedRestGuesses)
         (newWords : filter (/= word) previousWords : map (updateWordList word) restStates)

playHardTurn :: [GameState] -> IO () -> String -> IO ()
playHardTurn states start word = do
  guess <- toGuess word 0 <$> (putStrLn word >> inputColors)
  if all ((== Green) . color) guess
  then outputWinMessage "Hooray! I found the word!" start
  else chooseWordHard (updateComplexGameState states guess word) start

chooseWordHard :: [GameState] -> IO () -> IO ()
chooseWordHard states start
  | all null $ statesGuesses states = newStdGen >>= playHardTurn states start . chooseStartingWord (head $ statesWords states)
  | all null $ statesWords states = putStrLn "Your word is not part of the word list! Ending current session!"
  | otherwise =
      let words = filter (not . null) $ map findBestWord $ filter (not . null . snd) states
          bestWord = maximumBy (\w1 w2 -> compare (sum $ map (sumRemovedWords w1) states)
                                                  (sum $ map (sumRemovedWords w2) states)) words
      in playHardTurn states start bestWord