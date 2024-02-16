module Modes.Game where

import Control.Monad (when, unless)
import System.Random (newStdGen)
import Modes.Common (Guess, color, letter, outputWinMessage)
import Colors (findColors, findWrongColors)
import Utils (generateRandomNumber, makeSet)

validateWord :: Guess -> [Guess] -> IO ()
validateWord guess previousGuesses = do
  when
    (any (\x -> color x == "gray" && any (\y -> color x == color y && letter x == letter y) (concat previousGuesses)) guess)
    (putStrLn "Your guess contains grey letters that have been eliminated already")
  let yellows = foldl (\acc y -> if color y == "yellow" && all (\x -> (letter y, "green", x) `notElem` concat previousGuesses) [1 .. (length guess)] then y : acc else acc) [] (concat previousGuesses)
  when
    (any (\x -> not (any (\y -> (color y == "yellow" || color y == "green") && letter x == letter y) guess)) yellows)
    (putStrLn "Your guess is missing some of the yellow letters that have been found already")
  when
    (any (`notElem` guess) $ makeSet $ foldl (\acc y -> if color y == "green" then y : acc else acc) [] (concat previousGuesses))
    (putStrLn "Your guess has non green letter in a position, where a green letter was found")

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
  putStr "Enter your guess: "
  guess <- getLine
  checkWordLength guess secretWord (askForWordNormal secretWord words start)
  if guess == secretWord
    then outputWinMessage "Congratulations! You guessed the word!" start
    else do
      checkIfWordExists guess words (askForWordNormal secretWord words start)
      let result = map color $ findColors secretWord guess
      print result
      askForWordNormal secretWord words start

askForWordHard :: String -> [Guess] -> Bool -> [String] -> IO () -> IO ()
askForWordHard secretWord previousGuesses hasLied words start = do
  putStr "Enter your guess: "
  guess <- getLine
  checkWordLength guess secretWord (askForWordHard secretWord previousGuesses hasLied words start)
  generator <- newStdGen
  if guess == secretWord
    then outputWinMessage "Congratulations! You guessed the word!" start
    else do
      checkIfWordExists guess words (askForWordHard secretWord previousGuesses hasLied words start)
      let willLie = not hasLied && 20 >= generateRandomNumber generator 0 100
      if willLie
        then do
          colorGenerator <- newStdGen
          let result = findWrongColors secretWord guess previousGuesses colorGenerator
          print $ map color result
          askForWordHard secretWord previousGuesses True words start
        else do
          let result = map color $ findColors secretWord guess
          print result
          askForWordHard secretWord (findColors secretWord guess : previousGuesses) hasLied words start
