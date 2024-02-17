module Modes.Modes where

import System.Random (newStdGen)
import Modes.Game (askForWordEasy, askForWordNormal, askForWordHard)
import Modes.Helper (chooseWordNormal, chooseWordHard)
import Utils (stringToInt, generateRandomNumber)

chooseGameMode :: String -> String -> IO () -> IO ()
chooseGameMode contents "game" start = do
  difficulty <- putStr "Choose difficulty level (easy, normal, hard): " >> getLine
  if difficulty /= "easy" && difficulty /= "normal" && difficulty /= "hard"
    then putStrLn "Unrecognized difficulty" >> chooseGameMode contents "game" start
    else do
      wordLength <- putStr "Choose word length: " >> getLine
      let wordList = filter ((== stringToInt wordLength) . length) $ words contents
          wordCount = length wordList
      secretWord <- (wordList !!) . generateRandomNumber 0 (wordCount - 1) <$> newStdGen
      case difficulty of
        "easy" -> askForWordEasy secretWord [] wordList start
        "normal" -> askForWordNormal secretWord wordList start
        "hard" -> askForWordHard secretWord [] False wordList start

chooseGameMode contents "helper" start = do
  difficulty <- putStr "Choose difficulty level (normal, hard): " >> getLine
  if difficulty /= "normal" && difficulty /= "hard"
    then putStrLn "Unrecognized difficulty" >> chooseGameMode contents "helper" start
    else do
      wordLength <- putStr "Choose word length: " >> getLine
      let wordList = filter ((== stringToInt wordLength) . length) $ words contents
      case difficulty of
        "normal" -> chooseWordNormal ([], wordList) start
        "hard" -> chooseWordHard [([], wordList)] start

chooseGameMode contents _ start = do
  mode <- putStrLn "Unrecognized game mode" >> putStr "Choose game mode (game, helper): " >> getLine
  chooseGameMode contents mode start
