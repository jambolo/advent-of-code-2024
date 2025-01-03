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
import Day16 (day16_part1, day16_part2)
import Day17 (day17_part1, day17_part2)
import Day18 (day18_part1, day18_part2)
import Day19 (day19_part1, day19_part2)
import Day20 (day20_part1, day20_part2)
import Day21 (day21_part1, day21_part2)
import Day22 (day22_part1, day22_part2)
import Day23 (day23_part1, day23_part2)
import Day24 (day24_part1, day24_part2)
import Day25 (day25_part1, day25_part2)

type PuzzleInputToIntList = String -> IO [Int]

dayTable :: [(String, PuzzleInputToIntList)]
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
    ("day15_part2", day15_part2),
    ("day16_part1", day16_part1),
    ("day16_part2", day16_part2),
    ("day17_part1", day17_part1),
    ("day17_part2", day17_part2),
    ("day18_part1", day18_part1),
    ("day18_part2", day18_part2),
    ("day19_part1", day19_part1),
    ("day19_part2", day19_part2),
    ("day20_part1", day20_part1),
    ("day20_part2", day20_part2),
    ("day21_part1", day21_part1),
    ("day21_part2", day21_part2),
    ("day22_part1", day22_part1),
    ("day22_part2", day22_part2),
    ("day23_part1", day23_part1),
    ("day23_part2", day23_part2),
    ("day24_part1", day24_part1),
    ("day24_part2", day24_part2),
    ("day25_part1", day25_part1),
    ("day25_part2", day25_part2)
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
                Just solution -> do
                    timeIt $ do
                        result <- solution input
                        putStrLn $ day ++ " solution: " ++ show result
                Nothing -> error $ "Unknown day: " ++ day
        _ -> error "Usage: AdventOfCode2024 <day> <input file name>"
