module Utils where

import System.Random (RandomGen, Random (randomR))
import Data.Char (isDigit)
import Data.Maybe (isNothing)
import Control.Applicative (Applicative(liftA2))

makeSet :: Eq a => [a] -> [a]
makeSet = foldl (\res h -> if h `elem` res then res else res ++ [h]) []

digitToInt :: Char -> Maybe Int
digitToInt digit
  | isDigit digit = Just $ toEnum $ fromEnum digit - fromEnum '0'
  | otherwise = Nothing

stringToInt :: String -> Maybe Int
stringToInt string
  | any isNothing converted = Nothing
  | otherwise = foldl (liftA2 (+) . (<$>) (*10)) (Just 0) converted
  where converted = map digitToInt string

elemIndex :: Eq t => t -> [t] -> Maybe Int
elemIndex e list = elemIndexIter e list 0
    where elemIndexIter :: Eq t => t -> [t] -> Int -> Maybe Int
          elemIndexIter _ [] _ = Nothing
          elemIndexIter e (x:xs) index
            | e == x = Just index
            | otherwise = elemIndexIter e xs (index + 1)

generateRandomNumber :: RandomGen a => Int -> Int -> a -> Int
generateRandomNumber from to = fst . randomR (from, to)