{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
module Modes.Modes where

import System.Random (newStdGen)
import Utils (stringToInt, generateRandomNumber)
import Modes.Guesser.Normal (chooseWordNormal)
import Modes.Guesser.Hard (chooseWordHard)
import Modes.Classic.Easy (askForWordEasy)
import Modes.Classic.Normal (askForWordNormal)
import Modes.Classic.Hard (askForWordHard)
import Modes.Difficulties (
  parseClassicDifficulty, 
  ClassicDifficulty (ClassicEasy, 
                     ClassicNormal, 
                     ClassicHard), 
  parseGuesserDifficulty, 
  GuesserDifficulty (GuesserNormal, 
                     GuesserHard))

data GameMode = Classic | Guesser

parseGameMode :: String -> Maybe GameMode
parseGameMode "classic" = Just Classic
parseGameMode "Classic" = Just Classic
parseGameMode "guesser" = Just Guesser
parseGameMode "Guesser" = Just Guesser
parseGameMode _ = Nothing

enterGameMode :: IO GameMode
enterGameMode =
  putStr "Choose game mode (classic, guesser): " >> parseGameMode <$> getLine >>= \case
    Nothing -> putStrLn "Unrecognized game mode" >> enterGameMode
    Just value -> return value

filterWordList :: String -> IO [String]
filterWordList contents = 
  putStr "Choose word length: " >> getLine >>= \wordLength -> 
  case stringToInt wordLength of
    Nothing -> putStrLn "Incorrect word length" >> filterWordList contents
    Just lengthAsInt -> return $ filter ((== lengthAsInt) . length) $ words contents

chooseGameMode :: String -> GameMode -> IO () -> IO ()
chooseGameMode contents Classic start =
  putStr "Choose difficulty level (easy, normal, hard): " >> parseClassicDifficulty <$> getLine >>= \case
    Nothing -> putStrLn "Unrecognized difficulty" >> chooseGameMode contents Classic start
    Just difficulty -> do
      wordList <- filterWordList contents
      state <- (,wordList,start) . (wordList !!) . generateRandomNumber 0 (length wordList - 1) <$> newStdGen
      case difficulty of
        ClassicEasy -> askForWordEasy state []
        ClassicNormal -> askForWordNormal state
        ClassicHard -> askForWordHard state [] False

chooseGameMode contents Guesser start =
  putStr "Choose difficulty level (normal, hard): " >> parseGuesserDifficulty <$> getLine >>= \case
    Nothing -> putStrLn "Unrecognized difficulty" >> chooseGameMode contents Guesser start
    Just difficulty -> filterWordList contents >>= \wordList ->
      case difficulty of
        GuesserNormal -> chooseWordNormal ([], wordList) start
        GuesserHard -> chooseWordHard [([], wordList)] start