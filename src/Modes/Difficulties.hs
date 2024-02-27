module Modes.Difficulties where

data ClassicDifficulty = ClassicEasy | ClassicNormal | ClassicHard
data GuesserDifficulty = GuesserNormal | GuesserHard

parseClassicDifficulty :: String -> Maybe ClassicDifficulty
parseClassicDifficulty "easy" = Just ClassicEasy
parseClassicDifficulty "Easy" = Just ClassicEasy
parseClassicDifficulty "normal" = Just ClassicNormal
parseClassicDifficulty "Normal" = Just ClassicNormal
parseClassicDifficulty "hard" = Just ClassicHard
parseClassicDifficulty "Hard" = Just ClassicHard
parseClassicDifficulty _ = Nothing

parseGuesserDifficulty :: String -> Maybe GuesserDifficulty
parseGuesserDifficulty "normal" = Just GuesserNormal
parseGuesserDifficulty "Normal" = Just GuesserNormal
parseGuesserDifficulty "hard" = Just GuesserHard
parseGuesserDifficulty "Hard" = Just GuesserHard
parseGuesserDifficulty _ = Nothing