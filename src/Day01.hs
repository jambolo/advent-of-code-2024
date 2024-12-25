module Day01 (
    day01_part1,
    day01_part2
    ) where

import Data.List (sort)

sumOfDifferences :: ([Int], [Int]) -> Int
sumOfDifferences (xs, ys) = sum $ zipWith (\x y -> abs (x - y)) xs ys

parseLines :: String -> ([Int], [Int])
parseLines input = foldr parseLine ([], []) (lines input)
    where
        parseLine str (xs, ys) = case map read (words str) of
            [x, y] -> (x:xs, y:ys)
            _ -> (xs, ys)

sortPairs :: ([Int], [Int]) -> ([Int], [Int])
sortPairs (left, right) = (sort left, sort right)

day01_part1 :: String -> IO [Int]
day01_part1 input = do
    let pairs = parseLines input
--    print pairs
    let sorted = sortPairs pairs
--    print sorted
    let result = sumOfDifferences sorted
    return [result]


countOccurrences :: ([Int], [Int]) -> [(Int, Int)]
countOccurrences (left, right) = map (\l -> (l, count l right)) left
    where
        count x = length . filter (== x)

similarity :: [(Int, Int)] -> Int
similarity = foldr (\(x, y) acc -> x * y + acc) 0

day01_part2 :: String -> IO [Int]
day01_part2 input = do
    let pairs = parseLines input
--    print pairs
    let counts = countOccurrences pairs
--    print counts
    let result = similarity counts
    return [result]
