module Modes.Modes where

import System.Random (newStdGen)
import Modes.Game (askForWordEasy, askForWordNormal, askForWordHard)
import Modes.Helper (chooseWordNormal, chooseWordHard)
import Utils (stringToInt, generateRandomNumber)

chooseGameMode :: String -> String -> IO () -> IO ()
chooseGameMode contents "game" start = do
  putStr "Choose difficulty level (easy, normal, hard): "
  difficulty <- getLine
  if difficulty /= "easy" && difficulty /= "normal" && difficulty /= "hard"
    then putStrLn "Unrecognized difficulty" >> chooseGameMode contents "game" start
    else do
      putStr "Choose word length: "
      wordLength <- getLine
      generator <- newStdGen
      let wordList = filter (\x -> length x == stringToInt wordLength) $ words contents
          wordCount = length wordList
          secretWord = wordList !! generateRandomNumber generator 0 (wordCount - 1)
      case difficulty of
        "easy" -> askForWordEasy secretWord [] wordList start
        "normal" -> askForWordNormal secretWord wordList start
        "hard" -> askForWordHard secretWord [] False wordList start

chooseGameMode contents "helper" start = do
  putStr "Choose difficulty level (normal, hard): "
  difficulty <- getLine
  if difficulty /= "normal" && difficulty /= "hard"
    then putStrLn "Unrecognized difficulty" >> chooseGameMode contents "helper" start
    else do
      putStr "Choose word length: "
      wordLength <- getLine
      let wordList = filter (\x -> length x == stringToInt wordLength) $ words contents
      case difficulty of
        "normal" -> chooseWordNormal [] wordList start
        "hard" -> chooseWordHard [] [wordList] start

chooseGameMode contents _ start = do
  putStrLn "Unrecognized game mode" >> putStr "Choose game mode (game, helper): "
  mode <- getLine
  chooseGameMode contents mode start
