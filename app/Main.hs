module Main (main) where
    
import Data.Time.Clock (getCurrentTime, diffUTCTime)

import System.Environment (getArgs)
import Day01 (day01_part1, day01_part2)
import Day02 (day02_part1, day02_part2)
import Day03 (day03_part1, day03_part2)
import Day04 (day04_part1, day04_part2)
import Day05 (day05_part1, day05_part2)
import Day06 (day06_part1, day06_part2)
import Day07 (day07_part1, day07_part2)
import Day08 (day08_part1, day08_part2)
import Day09 (day09_part1, day09_part2)
import Day10 (day10_part1, day10_part2)
import Day11 (day11_part1, day11_part2)
import Day12 (day12_part1, day12_part2)
import Day13 (day13_part1, day13_part2)
import Day14 (day14_part1, day14_part2)
import Day15 (day15_part1, day15_part2)

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
    ("day05_part2", day05_part2),
    ("day06_part1", day06_part1),
    ("day06_part2", day06_part2),
    ("day07_part1", day07_part1),
    ("day07_part2", day07_part2),
    ("day08_part1", day08_part1),
    ("day08_part2", day08_part2),
    ("day09_part1", day09_part1),
    ("day09_part2", day09_part2),
    ("day10_part1", day10_part1),
    ("day10_part2", day10_part2),
    ("day11_part1", day11_part1),
    ("day11_part2", day11_part2),
    ("day12_part1", day12_part1),
    ("day12_part2", day12_part2),
    ("day13_part1", day13_part1),
    ("day13_part2", day13_part2),
    ("day14_part1", day14_part1),
    ("day14_part2", day14_part2),
    ("day15_part1", day15_part1),
    ("day15_part2", day15_part2)
    ]

timeIt :: IO a -> IO a
timeIt action = do
  start <- getCurrentTime
  result <- action
  end <- getCurrentTime
  putStrLn $ "Execution time: " ++ show (diffUTCTime end start)
  return result

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
                    timeIt $ do
                        result <- solution input
                        putStrLn $ day ++ " solution: " ++ show result
        _ -> error "Usage: AdventOfCode2024 <day> <input file name>"
