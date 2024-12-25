module Day19 (
    day19_part1,
    day19_part2
    ) where

--import Debug.Trace ( trace )
import Data.List.Split (splitOn)
import Data.List (dropWhileEnd)
import Data.Char (isSpace)

-- Trim leading and trailing whitespace
trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

-- Split and trim each part
splitAndTrim :: String -> [Char] -> [String]
splitAndTrim str delim = map trim (splitOn delim str)

loadData :: String -> ([String], [String])
loadData input =
    (splitAndTrim (head patterns) ",", tail designs)
    where
        (patterns, designs) = break null $ lines input

possible :: [String] -> String -> Bool
possible patterns design =
    any (\p ->
            let d0 = take (length p) design
                d1 = drop (length p) design
            in null design || (length p <= length design && p == d0 && possible patterns d1)
        ) patterns

-- Part 1
day19_part1 :: String -> IO [Int]
day19_part1 input = do
    let (patterns, designs) = loadData input
--    print patterns
--    print designs

    let result = length $ filter id $ map (possible patterns) designs
    return [result]

countPossible :: [String] -> String -> Int
countPossible patterns =
    count
    where
        count :: String -> Int
        count [] = 1
        count d =
            foldr (\p acc ->
                if length p <= length d && p == take (length p) d
                    then count (drop (length p) d) + acc
                    else acc
                ) 0 patterns

-- Part 2
day19_part2 :: String -> IO [Int]
day19_part2 input = do
    return []
