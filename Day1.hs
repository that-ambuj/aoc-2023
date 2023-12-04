module Day1 where

import Control.Applicative
import Data.Char (isDigit)
import Data.List
import Data.Maybe (mapMaybe)

main :: IO ()
main = interact $ show . solve2 . lines

solve1 :: [String] -> Int
solve1 = sum . map solveLine1

firstAndLast = read . (\xs -> [head xs, last xs])

solveLine1 :: String -> Int
solveLine1 = firstAndLast . filter isDigit

solve2 :: [String] -> Int
solve2 = sum . map solveLine2

solveLine2 :: String -> Int
solveLine2 = firstAndLast . mapMaybe convertPrefix . tails

convertPrefix :: String -> Maybe Char
convertPrefix [] = Nothing
convertPrefix s@(x : _)
  | "one" `isPrefixOf` s = Just '1'
  | "two" `isPrefixOf` s = Just '2'
  | "three" `isPrefixOf` s = Just '3'
  | "four" `isPrefixOf` s = Just '4'
  | "five" `isPrefixOf` s = Just '5'
  | "six" `isPrefixOf` s = Just '6'
  | "seven" `isPrefixOf` s = Just '7'
  | "eight" `isPrefixOf` s = Just '8'
  | "nine" `isPrefixOf` s = Just '9'
  | isDigit x = Just x
  | otherwise = Nothing
