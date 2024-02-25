module Modes.Game.Easy where

import Modes.Common (Guess, color, letter)
import Control.Monad (when, unless)
import Colors.Common (Color(Gray, Yellow, Green))
import Utils (makeSet)
import Modes.Game.State (wordExists, playTurn, Message)
import Colors.Colors (findColors)

containsGrayMessage :: Message
containsGrayMessage = "Your guess contains grey letters that have been eliminated already"
missingYellowMessage :: Message
missingYellowMessage = "Your guess is missing some of the yellow letters that have been found already"
missingGreenMessage :: Message
missingGreenMessage = "Your guess has non green letter in a position, where a green letter was found"

validateWord :: Guess -> [Guess] -> [Message]
validateWord guess previousGuesses = 
  map snd $ filter fst $ zip [containsGray, missesYellow, missesGreen] 
                             [containsGrayMessage, missingYellowMessage, missingGreenMessage]
    where allGuesses = concat previousGuesses
          isNotFoundAsGreen letter = all ((`notElem` concat (guess:previousGuesses)) . (,,) letter Green) [1 .. (length guess - 1)]
          yellows = filter (\ y -> color y == Yellow && isNotFoundAsGreen (letter y)) allGuesses
          containsGray = any (\x -> color x == Gray && any (\y -> color y == Gray && letter x == letter y) allGuesses) guess
          missesYellow = any (\x -> not $ any (\y -> color y == Yellow && letter x == letter y) guess) yellows
          missesGreen = any (`notElem` guess) $ makeSet $ filter ((== Green) . color) allGuesses

askForWordEasy :: String -> [String] -> IO () -> [Guess] -> IO ()
askForWordEasy secretWord words start previousGuesses =
  playTurn secretWord words start nextTurn action
    where nextTurn = askForWordEasy secretWord words start previousGuesses
          action guess = do
            let result = findColors secretWord guess
            mapM_ putStrLn $ validateWord result previousGuesses
            print $ map color result
            askForWordEasy secretWord words start (result : previousGuesses)