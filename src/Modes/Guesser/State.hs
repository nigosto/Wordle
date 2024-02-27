module Modes.Guesser.State where

import Modes.Common (Guess, letter, color, position)
import Utils (makeSet, generateRandomNumber)
import Colors.Common (Color(Green, Gray, Yellow), parseColor)
import Control.Arrow ((&&&))
import Colors.Colors (findColors)
import Data.List (maximumBy)
import Data.Maybe (isNothing)
import System.Random (RandomGen)

type GameState = ([Guess], [String])

statesGuesses :: [GameState] -> [[Guess]]
statesGuesses = map fst

statesWords :: [GameState] -> [[String]]
statesWords = map snd

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

chooseStartingWord::RandomGen a => [String] -> a -> String
chooseStartingWord words =
  let bestWords = filter (uncurry (==) . (length . makeSet &&& length)) words
  in (bestWords !!) . generateRandomNumber 0 (length bestWords - 1)
