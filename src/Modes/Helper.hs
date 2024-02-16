module Modes.Helper where

import Data.List (maximumBy)
import System.Random (newStdGen)
import Modes.Common (Guess, color, position, letter, outputWinMessage)
import Colors (findColors)
import Utils (makeSet, generateRandomNumber)

{--
- if the number of unique letters is equal to the number of letters in the secret word, then remove the word if it doesn't contain all of the letters
- if the word contains gray letter, remove it
- if the word doesn't contain green letter at the right position, remove it
- if the word doesn't contain yellow letter, remove it
--}
shouldRemoveWord :: [Guess] -> String -> Bool
shouldRemoveWord previousGuesses word =
  let green = makeSet $ filter (\x -> color x == "green") $ concat previousGuesses
      gray = makeSet $ filter (\x -> color x == "gray") $ concat previousGuesses
      yellow = makeSet $ filter (\x -> color x == "yellow" && x `notElem` green) $ concat previousGuesses
      areAllLettersFound = (not . null) previousGuesses && (length . head) previousGuesses == (length . makeSet . map letter) (green ++ yellow)
   in (areAllLettersFound && any (\x -> x `notElem` (makeSet . map letter) (green ++ yellow)) word)
        || any (`elem` (makeSet . map letter) gray) word
        || any (\x -> word !! (position x - 1) /= letter x) green
        || any (\x -> letter x `notElem` word) yellow

countRemovedWords :: String -> [String] -> String -> [Guess] -> Int
countRemovedWords currentWord words secretWord previousGuesses =
  let answer = findColors secretWord currentWord
   in length $ filter (shouldRemoveWord (answer : previousGuesses)) words

sumRemovedWords :: String -> [String] -> [Guess] -> Int
sumRemovedWords "" _ _ = 0
sumRemovedWords currentWord words previousGuesses = sum $ map (\w -> countRemovedWords currentWord words w previousGuesses) words--foldl (\res w -> res + countRemovedWords currentWord words w previousGuesses) 0 words

-- the check for empty words is required for other functions
findBestWord :: [String] -> [Guess] -> String
findBestWord [] _ = ""
findBestWord words previousGuesses = maximumBy (\ w1 w2 -> compare (sumRemovedWords w1 words previousGuesses) (sumRemovedWords w1 words previousGuesses)) words 

toGuess :: String -> [String] -> Int -> Guess
toGuess [] _ _ = []
toGuess (x : xs) (y : ys) index = (x, y, index) : toGuess xs ys (index + 1)

inputColors :: IO [String]
inputColors = do
  putStr "Please enter list of colors: "
  colors <- getLine
  let colorList = words colors
  if any (\x -> x /= "green" && x /= "gray" && x /= "yellow") colorList
    then putStrLn "Unrecognized colors! Try again!" >> inputColors
    else return colorList

{--
- The first word is always random word with all different letters (or else the first word will always be the same which is boring)
- After every word asks for colors
- Based of the new colors:
    - if the wordlist is empty after filtering then the word is not part of it and returns
    - if all the input colors are green then the secret word is found
    - else chooses the best word, filters the wordlist and goes over again
--}
chooseWordNormal :: [Guess] -> [String] -> IO () -> IO ()
chooseWordNormal [] words start = do
  generator <- newStdGen
  let wordsWithDifferentLetters = filter (\x -> length (makeSet x) == length x) words
      word = wordsWithDifferentLetters !! generateRandomNumber generator 0 (length wordsWithDifferentLetters - 1)
  putStrLn word
  colorList <- inputColors
  let guess = toGuess word colorList 1
  if all (\x -> color x == "green") guess
    then outputWinMessage "Hooray! I found the word!" start
    else chooseWordNormal [guess] (filter (\x -> not (shouldRemoveWord [guess] x) && x /= word) words) start

chooseWordNormal _ [] _ = putStrLn "Your word is not part of the word list! Ending current session!"

chooseWordNormal previousGuesses words start = do
  let word = findBestWord words previousGuesses
  putStrLn word
  colorList <- inputColors
  let guess = toGuess word colorList 1
  if all (\x -> color x == "green") guess
    then outputWinMessage "Hooray! I found the word!" start
    else chooseWordNormal (guess : previousGuesses) (filter (\x -> not (shouldRemoveWord (guess : previousGuesses) x) && x /= word) words) start

{- 
- chooseWordHard saves states of all guesses and available words for every guess where each state assumes that at some point the user lied (+ 1 state that
  assumes that the user hasn't lied yet)
- the heads of previousGuesses and listsOfWords represent the state where the function assumes that the user hasn't lied yet
- the second element is the state where the function assumes that the user lied about the current guess
- the rest states assume that the user has lied already 
-}
chooseWordHard :: [[Guess]] -> [[String]] -> IO () -> IO ()
chooseWordHard [] listsOfWords start = do
  generator <- newStdGen
  let wordsWithDifferentLetters = filter (\x -> length (makeSet x) == length x) $ head listsOfWords
      word = wordsWithDifferentLetters !! generateRandomNumber generator 0 (length wordsWithDifferentLetters - 1)
  putStrLn word
  colorList <- inputColors
  let guess = toGuess word colorList 1
  if all (\x -> color x == "green") guess
    then outputWinMessage "Hooray! I found the word!" start
    else chooseWordHard [[guess], []] (filter (\x -> not (shouldRemoveWord [guess] x) && x /= word) (head listsOfWords) : listsOfWords) start

{-
- the best word for every turn is calculate the same way like in the normal mode, but it is chosen from
  the best words of every state
-}
chooseWordHard previousGuesses listsOfWords start = do
  if all null listsOfWords
    then putStrLn "Your word is not part of the word list! Ending current session!"
    else do
      let states = zip previousGuesses listsOfWords
          words = map (\(guesses, wordList) -> findBestWord wordList guesses) $ filter (\(guesses, _) -> not $ null guesses) states
          bestWord = maximumBy (\w1 w2 -> compare (sum $ map (\(guesses, wordList) -> sumRemovedWords w1 wordList guesses) states)
                                                  (sum $ map (\(guesses, wordList) -> sumRemovedWords w2 wordList guesses) states))
                     $ filter (/= "") words
      putStrLn bestWord
      colorList <- inputColors
      let guess = toGuess bestWord colorList 1
      if all (\x -> color x == "green") guess
        then outputWinMessage "Hooray! I found the word!" start
        else chooseWordHard ((guess:head previousGuesses) :
                              head previousGuesses :
                              map (guess:) (tail previousGuesses))
                            (filter (\x -> not (shouldRemoveWord (guess:head previousGuesses) x) && x /= bestWord) (head listsOfWords) :
                             filter (/= bestWord) (head listsOfWords) :
                             map (\(guesses, wordList) -> filter (\w -> not (shouldRemoveWord (guess:guesses) w) && w /= bestWord) wordList) (tail states))
                             start
