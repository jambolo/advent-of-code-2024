module Day01
    ( day01_part1
    ) where

import Data.List (sort)
import Text.Read (readMaybe)

sumOfDifferences :: ([Int], [Int]) -> Int
sumOfDifferences (xs, ys) = sum $ zipWith (\x y -> abs (x - y)) xs ys

parseLines :: [String] -> ([Int], [Int])
parseLines = foldr parseLine ([], [])
  where
    parseLine str (xs, ys) =
        case words str of
            [a, b] -> case (readMaybe a, readMaybe b) of
                (Just x, Just y) -> (x:xs, y:ys)
                _ -> (xs, ys)
            _ -> (xs, ys)

sorted :: ([Int], [Int]) -> ([Int], [Int])
sorted (left, right) = (sort left, sort right)

day01_part1 :: [String] -> Int
day01_part1 linesOfInput = sumOfDifferences $ sorted $ parseLines linesOfInput
