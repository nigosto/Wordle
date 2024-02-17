module Colors.Common where

data Color = Green | Yellow | Gray deriving (Show, Enum, Eq)

parseColor :: String -> Maybe Color
parseColor "Green" = Just Green
parseColor "green" = Just Green
parseColor "gn" = Just Green
parseColor "Yellow" = Just Yellow
parseColor "yellow" = Just Yellow
parseColor "y" = Just Yellow
parseColor "Gray" = Just Gray
parseColor "gray" = Just Gray
parseColor "gy" = Just Gray
parseColor _ = Nothing