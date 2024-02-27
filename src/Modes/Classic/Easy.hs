module Modes.Classic.Easy where

import Modes.Common (Guess, color, letter)
import Control.Monad (when, unless)
import Colors.Common (Color(Gray, Yellow, Green))
import Utils (makeSet)
import Modes.Classic.State (wordExists, playTurn, Message, GameState, secretWord)
import Colors.Colors (findColors)

containingGrayMessage :: Message
containingGrayMessage = "Your guess contains grey letters that have been eliminated already"
missingYellowMessage :: Message
missingYellowMessage = "Your guess is missing some of the yellow letters that have been found already"
missingGreenMessage :: Message
missingGreenMessage = "Your guess has non green letter in a position, where a green letter was found"

validateWord :: Guess -> [Guess] -> [Message]
validateWord guess previousGuesses = 
  map snd $ filter fst $ zip [containsGray, missesYellow, missesGreen] 
                             [containingGrayMessage, missingYellowMessage, missingGreenMessage]
    where allGuesses = concat previousGuesses
          isNotFoundAsGreen letter = all ((`notElem` concat (guess:previousGuesses)) . (,,) letter Green) [1 .. (length guess - 1)]
          yellows = filter (\ y -> color y == Yellow && isNotFoundAsGreen (letter y)) allGuesses
          containsGray = any (\x -> color x == Gray && any (\y -> color y == Gray && letter x == letter y) allGuesses) guess
          missesYellow = any (\x -> not $ any (\y -> color y == Yellow && letter x == letter y) guess) yellows
          missesGreen = any (`notElem` guess) $ makeSet $ filter ((== Green) . color) allGuesses

askForWordEasy :: GameState -> [Guess] -> IO ()
askForWordEasy state previousGuesses =
  playTurn state nextTurn action
    where nextTurn = askForWordEasy state previousGuesses
          action guess = do
            let newGuess = findColors (secretWord state) guess
            mapM_ putStrLn $ validateWord newGuess previousGuesses
            print $ map color newGuess
            askForWordEasy state (newGuess : previousGuesses)