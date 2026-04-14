module Day19 (
    day19_part1,
    day19_part2
    ) where

--import Debug.Trace ( trace )
import Data.List.Split (splitOn)
import Data.List (dropWhileEnd)
import Data.Char (isSpace)

import Answer (Answer(..))

-- Trim leading and trailing whitespace
trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

-- Split and trim each part
splitAndTrim :: String -> [Char] -> [String]
splitAndTrim str delim = map trim (splitOn delim str)

loadData :: String -> ([String], [String])
loadData input =
    let (patterns, designs) = break null $ lines input
    in case (patterns, designs) of
        (p:_, _:d) -> (splitAndTrim p ",", d)
        _         -> error "Invalid input to loadData"

possible :: [String] -> String -> Bool
possible patterns design =
    any (\p ->
            let d0 = take (length p) design
                d1 = drop (length p) design
            in null design || (length p <= length design && p == d0 && possible patterns d1)
        ) patterns

-- Part 1
day19_part1 :: String -> IO Answer
day19_part1 input = do
    let (patterns, designs) = loadData input
--    print patterns
--    print designs

    let result = length $ filter id $ map (possible patterns) designs
    return (Ints [result])

-- Part 2
day19_part2 :: String -> IO Answer
day19_part2 _input = do
    return (Ints [])
