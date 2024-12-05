module Day01
    ( day01_part1
    , day01_part2
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
day01_part1 inputLines = sumOfDifferences $ sorted $ parseLines inputLines

countOccurrences :: ([Int], [Int]) -> [(Int, Int)]
countOccurrences (left, right) = map (\x -> (x, count x right)) left
    where
        count x = length . filter (== x)

similarity :: [(Int, Int)] -> Int
similarity = foldr (\(x, y) acc -> x * y + acc) 0

day01_part2 :: [String] -> Int
day01_part2 inputLines = similarity $ countOccurrences $ parseLines inputLines
