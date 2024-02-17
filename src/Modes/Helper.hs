module Modes.Helper where

import Control.Arrow ((&&&))
import Data.Maybe (isNothing)
import Data.List (maximumBy)
import System.Random (newStdGen, RandomGen)
import Modes.Common (Guess, color, position, letter, outputWinMessage)
import Colors.Colors (findColors)
import Utils (makeSet, generateRandomNumber)
import Colors.Common (Color (Green, Yellow, Gray), parseColor)

type GameState = ([Guess], [String])

statesGuesses :: [GameState] -> [[Guess]]
statesGuesses = map fst

statesWords :: [GameState] -> [[String]]
statesWords = map snd

{--
- if the number of unique letters is equal to the number of letters in the secret word,
  then remove the word if it doesn't contain all of the letters
- if the word contains gray letter, remove it
- if the word doesn't contain green letter at the right position, remove it
- if the word doesn't contain yellow letter, remove it
--}
shouldRemoveWord :: [Guess] -> String -> Bool
shouldRemoveWord previousGuesses word =
  let green = makeSet $ filter ((== Green) . color) $ concat previousGuesses
      gray = makeSet $ filter ((== Gray) . color) $ concat previousGuesses
      yellow = makeSet $ filter (uncurry (&&) . ((== Yellow) . color &&& (`notElem` green))) $ concat previousGuesses
      areAllLettersFound = (not . null) previousGuesses && (length . head) previousGuesses == (length . makeSet . map letter) (green ++ yellow)
  in areAllLettersFound && any (`notElem` (makeSet . map letter) (green ++ yellow)) word ||
     any (`elem` (makeSet . map letter) gray) word ||
     any (uncurry (/=) . ((!!) word . position &&& letter)) green ||
     any ((`notElem` word) . letter) yellow

countRemovedWords :: String -> GameState -> String -> Int
countRemovedWords currentWord (previousGuesses, words) secretWord =
  let answer = findColors secretWord currentWord
  in length $ filter (shouldRemoveWord (answer : previousGuesses)) words

sumRemovedWords :: String -> GameState -> Int
sumRemovedWords "" _ = 0
sumRemovedWords currentWord state@(_, words) = sum $ map (countRemovedWords currentWord state) words

-- the check for empty words is required for other functions
findBestWord :: GameState -> String
findBestWord ([], _) = ""
findBestWord state@(_, words) = maximumBy (\ w1 w2 -> compare (sumRemovedWords w1 state) (sumRemovedWords w1 state)) words

toGuess :: String -> Int -> [String] -> Guess
toGuess [] _ _ = []
toGuess (x : xs) index (y : ys) =
  case parseColor y of
  Just color -> (x, color, index) : toGuess xs (index + 1) ys
  Nothing -> error "Invalid color"

inputColors :: IO [String]
inputColors = do
  putStr "Please enter list of colors: "
  colorList <- words <$> getLine
  if any (isNothing . parseColor) colorList
  then putStrLn "Unrecognized colors! Try again!" >> inputColors
  else return colorList

updateWordList :: String -> GameState -> [String]
updateWordList word (guesses, words) = filter (uncurry (&&) . (not . shouldRemoveWord guesses &&& (/= word))) words

updateGameState :: GameState -> Guess -> String -> GameState
updateGameState (guesses, words) guess word =
  let updatedGuesses = guess : guesses
  in (updatedGuesses, updateWordList word (updatedGuesses, words))

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

chooseStartingWord::RandomGen a => [String] -> a -> String
chooseStartingWord words =
  let bestWords = filter (uncurry (==) . (length . makeSet &&& length)) words
  in (bestWords !!) . generateRandomNumber 0 (length bestWords - 1)

playNormalTurn :: GameState -> IO () -> String -> IO ()
playNormalTurn state start word = do
  guess <- toGuess word 0 <$> (putStrLn word >> inputColors)
  if all ((== Green) . color) guess
    then outputWinMessage "Hooray! I found the word!" start
    else chooseWordNormal (updateGameState state guess word) start

playHardTurn :: [GameState] -> IO () -> String -> IO ()
playHardTurn states start word = do
  guess <- toGuess word 0 <$> (putStrLn word >> inputColors)
  if all ((== Green) . color) guess
  then outputWinMessage "Hooray! I found the word!" start
  else chooseWordHard (updateComplexGameState states guess word) start

{--
- The first word is always random word with all different letters (or else the first word will always be the same which is boring)
- After every word asks for colors
- Based of the new colors:
    - if the wordlist is empty after filtering then the word is not part of it and returns
    - if all the input colors are green then the secret word is found
    - else chooses the best word, filters the wordlist and goes over again
--}
chooseWordNormal :: GameState -> IO () -> IO ()
chooseWordNormal state@([], words) start = newStdGen >>= playNormalTurn state start . chooseStartingWord words
chooseWordNormal (_, []) _ = putStrLn "Your word is not part of the word list! Ending current session!"
chooseWordNormal state start = playNormalTurn state start $ findBestWord state

{- 
- chooseWordHard saves states of all guesses and available words for every guess where each state assumes that at some point the user lied 
  (+ 1 state that assumes that the user hasn't lied yet)
- the first state represents the state where the function assumes that the user hasn't lied yet
- the second state is the state where the function assumes that the user lied about the current guess
- the rest states assume that the user has lied already 
-}
chooseWordHard :: [GameState] -> IO () -> IO ()
chooseWordHard states start
  | all null $ statesGuesses states = newStdGen >>= playHardTurn states start . chooseStartingWord (head $ statesWords states)
  | all null $ statesWords states = putStrLn "Your word is not part of the word list! Ending current session!"
  | otherwise = do
      let words = filter (not . null) $ map findBestWord $ filter (not . null . snd) states
          bestWord = maximumBy (\w1 w2 -> compare (sum $ map (sumRemovedWords w1) states) 
                                                  (sum $ map (sumRemovedWords w2) states)) words
      playHardTurn states start bestWord