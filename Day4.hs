module Day4 where

import Data.Bifunctor (bimap)
import Data.List (iterate)
import Data.List.Split (splitOn)

main :: IO ()
main = interact $ show . solve2' . lines

solve1 :: [String] -> Int
solve1 = sum . map solveLine1

listToTuples :: [a] -> [(Int, a)]
listToTuples = zip (repeat 1)

solve2' :: [String] -> Int
solve2' = solve2 0 . listToTuples

solve2 :: Int -> [(Int, String)] -> Int
solve2 score [] = score
solve2 score ((i, x) : xs) = solve2 (score + i) $ rewards' ++ rest
  where
    wins = winningNums x
    winCount = length wins
    (rewards, rest) = splitAt winCount xs
    rewards' = map (\(r, c) -> (r + i, c)) rewards

solveLine1 :: String -> Int
solveLine1 = foldr f 0 . winningNums
  where
    f _ 0 = 1
    f _ acc = acc * 2

winningNums :: String -> [String]
winningNums str = filter (`elem` winning) nums
  where
    [_, rest] = splitOn ":" str
    [winning, nums] = map words $ splitOn "|" rest
