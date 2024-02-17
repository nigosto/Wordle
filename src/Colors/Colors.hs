module Colors.Colors where

import Data.Maybe (isJust)
import System.Random (Random (randomR, randomRs), RandomGen)
import Colors.Common (Color (Green, Yellow, Gray))
import Modes.Common (Guess, LetterInfo, letter)
import Utils (makeSet, elemIndex)

generateRandomColor :: RandomGen a => a -> Int -> Color
generateRandomColor generator =
  toEnum . generateRandomNumberList generator 0 2
  where
    generateRandomNumberList generator from to index = randomRs (from, to) generator !! index

mapLetterToColor :: String -> Char -> Int -> LetterInfo
mapLetterToColor secretWord letter index
  | letter `elemIndex` secretWord == Just index = (letter, Green, index)
  | isJust $ letter `elemIndex` secretWord = (letter, Yellow, index)
  | otherwise = (letter, Gray, index)

findColors :: String -> String -> Guess
findColors secretWord guess = zipWith (mapLetterToColor secretWord) guess [0..length guess - 1]
  
findWrongColors :: RandomGen a => String -> String -> [Guess] -> a -> Guess
findWrongColors secretWord guess previousGuesses generator =
  findWrongLettersColors secretWord guess 0 0
    where findWrongLettersColors :: String -> String -> Int -> Int -> Guess
          findWrongLettersColors _  [] _ _ = []
          findWrongLettersColors secretWord (x:xs) index count
            | x `elem` previousLetters = mapLetterToColor secretWord x index : findWrongLettersColors secretWord xs (index + 1) count
            | otherwise = (x, generateRandomColor generator count, index) : findWrongLettersColors secretWord xs (index + 1) (count + 1)
          previousLetters :: String
          previousLetters = makeSet $ map letter $ concat previousGuesses