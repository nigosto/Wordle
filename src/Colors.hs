module Colors where

import System.Random (Random (randomR, randomRs), RandomGen)
import Modes.Common (Guess, letter)
import Utils (makeSet) 

data Color = Green | Yellow | Gray deriving (Show, Read, Enum)

generateRandomColor :: RandomGen a => a -> Int -> String
generateRandomColor generator count =
  let colors = ["green", "yellow", "gray"]
   in colors !! generateRandomNumberList generator 0 2 count
  where
    generateRandomNumberList generator from to index = randomRs (from, to) generator !! index

findColors :: String -> String -> Guess
findColors secretWord guess = helper secretWord guess secretWord 1
  where
    helper currSecretWord guess secretWord pos
      | null guess = []
      | head guess == head currSecretWord = (head guess, "green", pos) : helper (tail currSecretWord) (tail guess) secretWord (pos + 1)
      | head guess /= head currSecretWord && head guess `elem` secretWord = (head guess, "yellow", pos) : helper (tail currSecretWord) (tail guess) secretWord (pos + 1)
      | otherwise = (head guess, "gray", pos) : helper (tail currSecretWord) (tail guess) secretWord (pos + 1)

findWrongColors :: RandomGen a => String -> String -> [Guess] -> a -> Guess
findWrongColors secretWord guess previousGuesses generator =
  let previousLetters = makeSet $ map letter $ concat previousGuesses
   in helper secretWord guess secretWord 1 previousLetters generator 0
  where
    helper currSecretWord guess secretWord pos previousLetters generator count
      | null guess = []
      | head guess `elem` previousLetters && head guess == head currSecretWord = (head guess, "green", pos) : helper (tail currSecretWord) (tail guess) secretWord (pos + 1) previousLetters generator count
      | head guess `elem` previousLetters && head guess /= head currSecretWord && head guess `elem` secretWord = (head guess, "yellow", pos) : helper (tail currSecretWord) (tail guess) secretWord (pos + 1) previousLetters generator count
      | head guess `elem` previousLetters && head guess `notElem` secretWord = (head guess, "gray", pos) : helper (tail currSecretWord) (tail guess) secretWord (pos + 1) previousLetters generator count
      | otherwise = (head guess, generateRandomColor generator count, pos) : helper (tail currSecretWord) (tail guess) secretWord (pos + 1) previousLetters generator (count + 1)
