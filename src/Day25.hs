module Day25 (
    day25_part1,
    day25_part2
    ) where

import Data.List (transpose)
import Data.List.Split (splitOn)

parseSchematic :: [String] -> [Int]
parseSchematic s = 
    map (length . filter (== '#')) $ transpose (init (tail s))

loadSchematics :: String -> ([[Int]], [[Int]])
loadSchematics input = foldr store ([], []) (splitOn [""] $ lines input)
    where store s (key, lock)
            | head s == "#####" = (key, heights:lock)
            | otherwise         = (heights:key, lock)
            where heights = parseSchematic s

fits :: [Int] -> [Int] -> Bool
fits key lock = 
    all (\(k, l) -> k + l <= 5) $ zip key lock

day25_part1 :: String -> IO [Int]
day25_part1 input = do
    let (keys, locks) = loadSchematics input
--    print (keys, locks)
    let result = length [() | key <- keys, lock <- locks, fits key lock]
    return [result]

-- Part 2
day25_part2 :: String -> IO [Int]
day25_part2 _ = do
    return []
