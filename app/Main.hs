module Main (main) where

import System.Environment (getArgs)
import Day01 (day01_part1, day01_part2)
import Day02 (day02_part1, day02_part2)
import Day03 (day03_part1, day03_part2)
import Day04 (day04_part1, day04_part2)
import Day05 (day05_part1, day05_part2)

type PuzzleInputToInt = String -> IO Int

dayTable :: [(String, PuzzleInputToInt)]
dayTable = [
    ("day01_part1", day01_part1),
    ("day01_part2", day01_part2),
    ("day02_part1", day02_part1),
    ("day02_part2", day02_part2),
    ("day03_part1", day03_part1),
    ("day03_part2", day03_part2),
    ("day04_part1", day04_part1),
    ("day04_part2", day04_part2),
    ("day05_part1", day05_part1),
    ("day05_part2", day05_part2)
    ]

main :: IO ()
main = do
    args <- getArgs
    case args of
        [day, inputName] -> do
            putStrLn $ "Day " ++ day ++ ", input: " ++ inputName
            input <- readFile inputName
            case lookup day dayTable of
                Nothing -> error $ "Unknown day: " ++ day
                Just solution -> do
                    result <- solution input
                    putStrLn $ day ++ " solution: " ++ show result
          
        _ -> error "Usage: AdventOfCode2024 <day> <input file name>"
