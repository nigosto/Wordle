module Modes.Game where

import Control.Monad (when, unless)
import System.Random (newStdGen)
import Modes.Common (Guess, LetterInfo, color, letter, outputWinMessage)
import Colors.Common (Color (Green, Yellow, Gray))
import Colors.Colors (findColors, findWrongColors)
import Utils (generateRandomNumber, makeSet)

validateWord :: Guess -> [Guess] -> IO ()
validateWord guess previousGuesses = do
  when
    (any (\x -> color x == Gray && any (\y -> color y == Gray && letter x == letter y) allGuesses) guess) $
    putStrLn "Your guess contains grey letters that have been eliminated already"
  let yellows = filter (\ y -> color y == Yellow && checkIfLetterIsFoundAsGreen (letter y)) allGuesses
  unless (all (\x -> any (\y -> color y == Yellow && letter x == letter y) guess) yellows) $
    putStrLn "Your guess is missing some of the yellow letters that have been found already"
  when
    (any (`notElem` guess) $ makeSet $ filter ((== Green) . color) allGuesses) $
    putStrLn "Your guess has non green letter in a position, where a green letter was found"
  where checkIfLetterIsFoundAsGreen :: Char -> Bool
        checkIfLetterIsFoundAsGreen letter = all (\x -> (letter, Green, x) `notElem` allGuesses) [1 .. (length guess - 1)]
        allGuesses :: [LetterInfo]
        allGuesses = concat previousGuesses

checkWordLength :: String -> String -> IO () -> IO ()
checkWordLength guess secretWord =
  when (length guess /= length secretWord) . (>>) (putStr "Your word is not " >> (putStr . show $ length secretWord) >> putStrLn " symbols.")

checkIfWordExists :: String -> [String] -> IO () -> IO ()
checkIfWordExists guess words =
  unless (guess `elem` words) . (>>) (putStrLn "Your guess is not part of the wordlist")

askForWordEasy :: String -> [Guess] -> [String] -> IO () -> IO ()
askForWordEasy secretWord previousGuesses words start = do
  putStr "Enter your guess: "
  guess <- getLine
  checkWordLength guess secretWord (askForWordEasy secretWord previousGuesses words start)
  if guess == secretWord
    then outputWinMessage "Congratulations! You guessed the word!" start
    else do
      checkIfWordExists guess words (askForWordEasy secretWord previousGuesses words start)
      let result = findColors secretWord guess
      validateWord result previousGuesses
      print $ map color result
      askForWordEasy secretWord (result : previousGuesses) words start

askForWordNormal :: String -> [String] -> IO () -> IO ()
askForWordNormal secretWord words start = do
  guess <- putStr "Enter your guess: " >> getLine
  checkWordLength guess secretWord (askForWordNormal secretWord words start)
  if guess == secretWord
    then outputWinMessage "Congratulations! You guessed the word!" start
    else do
      checkIfWordExists guess words (askForWordNormal secretWord words start)
      print $ map color $ findColors secretWord guess
      askForWordNormal secretWord words start

askForWordHard :: String -> [Guess] -> Bool -> [String] -> IO () -> IO ()
askForWordHard secretWord previousGuesses hasLied words start = do
  guess <- putStr "Enter your guess: " >> getLine
  checkWordLength guess secretWord (askForWordHard secretWord previousGuesses hasLied words start)
  generator <- newStdGen
  if guess == secretWord
    then outputWinMessage "Congratulations! You guessed the word!" start
    else do
      checkIfWordExists guess words (askForWordHard secretWord previousGuesses hasLied words start)
      let willLie = not hasLied && 20 >= generateRandomNumber generator 0 100
      if willLie
        then do
          newStdGen >>= print . map color . findWrongColors secretWord guess previousGuesses
          askForWordHard secretWord previousGuesses True words start
        else do
          print $ map color $ findColors secretWord guess
          askForWordHard secretWord (findColors secretWord guess : previousGuesses) hasLied words start
