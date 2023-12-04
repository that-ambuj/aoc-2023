module Day2 where

import Data.Char (isDigit)
import Data.List (isSuffixOf)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)

data Game = Game {idx :: Int, rounds :: [GameRound]} deriving (Show, Eq)

data GameRound = GameRound
  { red :: Int,
    green :: Int,
    blue :: Int
  }
  deriving (Show, Eq)

main :: IO ()
main = interact $ show . solve2 . lines

solve1 :: [String] -> Int
solve1 =
  sum
    . map idx
    . filter possibleGame
    . map parseGame

possibleGame :: Game -> Bool
possibleGame = all (\(GameRound r g b) -> r <= 12 && g <= 13 && b <= 14) . rounds

solve2 :: [String] -> Int
solve2 =
  sum
    . map
      ( (\(GameRound r g b) -> r * g * b)
          . fewestCubesEach
          . rounds
          . parseGame
      )

fewestCubesEach :: [GameRound] -> GameRound
fewestCubesEach =
  foldr
    (\(GameRound r g b) (GameRound r' g' b') -> GameRound (max r r') (max g g') (max b b'))
    (GameRound 0 0 0)

parseGame :: String -> Game
parseGame str = Game idx rounds
  where
    [start, rest] = splitOn ":" str
    idx = extractNumber' start
    rounds = parseRounds rest

extractNumber :: String -> Maybe Int
extractNumber = readMaybe . filter isDigit

extractNumber' :: String -> Int
extractNumber' = read . filter isDigit

parseRounds :: String -> [GameRound]
parseRounds = map parseRound . splitOn ";"

parseRound :: String -> GameRound
parseRound = parseRound' (GameRound 0 0 0) . splitOn ","

parseRound' :: GameRound -> [String] -> GameRound
parseRound' g [] = g
parseRound' (GameRound r g b) ys@(x : xs)
  | "red" `isSuffixOf` x = parseRound' (GameRound n g b) xs
  | "green" `isSuffixOf` x = parseRound' (GameRound r n b) xs
  | "blue" `isSuffixOf` x = parseRound' (GameRound r g n) xs
  | otherwise = GameRound r g b
  where
    n = fromMaybe 0 $ extractNumber x
