import System.Random (newStdGen, Random (randomR, randomRs), RandomGen)
import Data.Maybe (isJust)

data Color = Green | Yellow | Gray deriving (Show, Enum)
type LetterInfo = (Char, Color, Int)
type Guess = [LetterInfo]

letter :: LetterInfo -> Char
letter (l, _, _) = l

makeSet :: Eq a => [a] -> [a]
makeSet = foldl (\res h -> if h `elem` res then res else res ++ [h]) []

generateRandomColor :: RandomGen a => a -> Int -> Color
generateRandomColor generator count =
  toEnum $ generateRandomNumberList generator 0 2 count
  where
    generateRandomNumberList generator from to index = randomRs (from, to) generator !! index

elemIndex :: Eq t => t -> [t] -> Maybe Int
elemIndex e list = elemIndexIter e list 0
    where elemIndexIter :: Eq t => t -> [t] -> Int -> Maybe Int
          elemIndexIter _ [] _ = Nothing
          elemIndexIter e (x:xs) index
            | e == x = Just index
            | otherwise = elemIndexIter e xs (index + 1)

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

main:: IO()
main = do
    generator <- newStdGen
    print $ findWrongColors "bread" "about" [[('b', Green, 0), ('u', Gray, 1), ('n', Gray, 2), ('c', Gray, 3), ('h', Gray, 4)]] generator