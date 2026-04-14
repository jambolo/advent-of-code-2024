module Day25 (
    day25_part1,
    day25_part2
    ) where

import Data.List (transpose)
import Data.List.Split (splitOn)

import Answer (Answer(..))

parseSchematic :: [String] -> [Int]
parseSchematic s =
    let s' = case s of
            (_:xs@(_:_)) -> init xs
            _            -> []
    in map (length . filter (== '#')) $ transpose s'

loadSchematics :: String -> ([[Int]], [[Int]])
loadSchematics input = foldr store ([], []) (splitOn [""] $ lines input)
    where
        store s (keys, locks) =
            let heights = parseSchematic s
            in if take 1 s == ["#####"]
               then (keys, heights : locks)
               else (heights : keys, locks)

fits :: [Int] -> [Int] -> Bool
fits key lock =
    all (\(k, l) -> k + l <= 5) $ zip key lock

day25_part1 :: String -> IO Answer
day25_part1 input = do
    let (keys, locks) = loadSchematics input
--    print (keys, locks)
    let result = length [() | key <- keys, lock <- locks, fits key lock]
    return (Ints [result])

-- Part 2
day25_part2 :: String -> IO Answer
day25_part2 _ = do
    return (Ints [])
