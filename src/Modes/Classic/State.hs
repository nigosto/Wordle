module Modes.Classic.State where

import Control.Monad (when, unless)
import Modes.Common (outputWinMessage, color)
import Data.Maybe (isNothing)
import Colors.Colors (findColors)

type GameState = (String, [String], IO ())
type Message = String

secretWord :: GameState -> String
secretWord (word, _, _) = word

wordIsCorrectLength :: String -> String -> Maybe Message
wordIsCorrectLength guess secretWord
  | length guess /= length secretWord = Just $ concat ["Your word is not ", show $ length secretWord, " symbols."] 
  | otherwise = Nothing

wordExists :: String -> [String] -> Maybe Message
wordExists guess words
  | guess `notElem` words = Just "Your guess is not part of the wordlist"
  | otherwise = Nothing

showErrorMessage :: Maybe Message -> IO () -> IO () -> IO ()
showErrorMessage Nothing _ action = action
showErrorMessage (Just message) next _ = putStrLn message >> next

playTurn :: GameState -> IO () -> (String -> IO ()) -> IO ()
playTurn (secretWord, words, start) next action =
  putStr "Enter your guess: " >> getLine >>= \guess ->
  showErrorMessage (wordIsCorrectLength guess secretWord) next $
    if guess == secretWord
    then outputWinMessage "Congratulations! You guessed the word!" start
    else showErrorMessage (wordExists guess words) next $ action guess