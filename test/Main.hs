module Main(main) where

import Test.HUnit
import System.IO.Unsafe

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

assertNotImplemented :: String -> [Int] -> [Int] -> Assertion
assertNotImplemented preface expected actual
  | not (null expected) = assertFailure $ "Using assertNotImplemented with a non-empty expected"
  | null actual = assertFailure $ preface ++ " is not implemented yet"
  | otherwise   = assertFailure $ preface ++ " is assumed to be unimplemented, but returned " ++ show actual

run :: (String -> IO [Int]) -> String -> [Int]
run day file = unsafePerformIO (do { input <- readFile file; day input })

test01_1 :: Test
test01_1 = TestCase (assertEqual "Day 01, part 1" [2192892]           (run day01_part1 "input/day01-input.txt"))
test01_2 :: Test
test01_2 = TestCase (assertEqual "Day 01, part 2" [22962826]          (run day01_part2 "input/day01-input.txt"))
test02_1 :: Test
test02_1 = TestCase (assertEqual "Day 02, part 1" [371]               (run day02_part1 "input/day02-input.txt"))
test02_2 :: Test
test02_2 = TestCase (assertEqual "Day 02, part 2" [426]               (run day02_part2 "input/day02-input.txt"))
test03_1 :: Test
test03_1 = TestCase (assertEqual "Day 03, part 1" [166630675]         (run day03_part1 "input/day03-input.txt"))
test03_2 :: Test
test03_2 = TestCase (assertEqual "Day 03, part 2" [93465710]          (run day03_part2 "input/day03-input.txt"))
test04_1 :: Test
test04_1 = TestCase (assertEqual "Day 04, part 1" [2464]              (run day04_part1 "input/day04-input.txt"))
test04_2 :: Test
test04_2 = TestCase (assertEqual "Day 04, part 2" [1982]              (run day04_part2 "input/day04-input.txt"))
test05_1 :: Test
test05_1 = TestCase (assertEqual "Day 05, part 1" [5091]              (run day05_part1 "input/day05-input.txt"))
test05_2 :: Test
test05_2 = TestCase (assertEqual "Day 05, part 2" [4681]              (run day05_part2 "input/day05-input.txt"))
test06_1 :: Test
test06_1 = TestCase (assertEqual "Day 06, part 1" [4939]              (run day06_part1 "input/day06-input.txt"))
test06_2 :: Test
test06_2 = TestCase (assertEqual "Day 06, part 2" [1434]              (run day06_part2 "input/day06-input.txt"))
test07_1 :: Test
test07_1 = TestCase (assertEqual "Day 07, part 1" [3119088655389]     (run day07_part1 "input/day07-input.txt"))
test07_2 :: Test
test07_2 = TestCase (assertEqual "Day 07, part 2" [264184041398847]   (run day07_part2 "input/day07-input.txt"))
test08_1 :: Test
test08_1 = TestCase (assertEqual "Day 08, part 1" [392]               (run day08_part1 "input/day08-input.txt"))
test08_2 :: Test
test08_2 = TestCase (assertEqual "Day 08, part 2" [1235]              (run day08_part2 "input/day08-input.txt"))
test09_1 :: Test
test09_1 = TestCase (assertEqual "Day 09, part 1" [6378826667552]     (run day09_part1 "input/day09-input.txt"))
test09_2 :: Test
test09_2 = TestCase (assertEqual "Day 09, part 2" [6413328569890]     (run day09_part2 "input/day09-input.txt"))
test10_1 :: Test
test10_1 = TestCase (assertEqual "Day 10, part 1" [587]               (run day10_part1 "input/day10-input.txt"))
test10_2 :: Test
test10_2 = TestCase (assertEqual "Day 10, part 2" [1340]              (run day10_part2 "input/day10-input.txt"))
test11_1 :: Test
test11_1 = TestCase (assertEqual "Day 11, part 1" [202019]            (run day11_part1 "input/day11-input.txt"))
test11_2 :: Test
test11_2 = TestCase (assertEqual "Day 11, part 2" [239321955280205]   (run day11_part2 "input/day11-input.txt"))
test12_1 :: Test
test12_1 = TestCase (assertEqual "Day 12, part 1" [1477924]           (run day12_part1 "input/day12-input.txt"))
test12_2 :: Test
test12_2 = TestCase (assertNotImplemented "Day 12, part 2" []         (run day12_part2 "input/day12-input.txt"))
test13_1 :: Test
test13_1 = TestCase (assertEqual "Day 13, part 1" [29023]             (run day13_part1 "input/day13-input.txt"))
test13_2 :: Test
test13_2 = TestCase (assertNotImplemented "Day 13, part 2" []         (run day13_part2 "input/day13-input.txt"))
test14_1 :: Test
test14_1 = TestCase (assertEqual "Day 14, part 1" [230900224]         (run day14_part1 "input/day14-input.txt"))
test14_2 :: Test
test14_2 = TestCase (assertNotImplemented "Day 14, part 2" []         (run day14_part2 "input/day14-input.txt"))
test15_1 :: Test
test15_1 = TestCase (assertEqual "Day 15, part 1" [1451928]           (run day15_part1 "input/day15-input.txt"))
test15_2 :: Test
test15_2 = TestCase (assertNotImplemented "Day 15, part 2" []         (run day15_part2 "input/day15-input.txt"))
test16_1 :: Test
test16_1 = TestCase (assertEqual "Day 16, part 1" [133584]            (run day16_part1 "input/day16-input.txt"))
test16_2 :: Test
test16_2 = TestCase (assertNotImplemented "Day 16, part 2" []         (run day16_part2 "input/day16-input.txt"))
test17_1 :: Test
test17_1 = TestCase (assertEqual "Day 17, part 1" [7,3,5,7,5,7,4,3,0] (run day17_part1 "input/day17-input.txt"))
test17_2 :: Test
test17_2 = TestCase (assertEqual "Day 17, part 2" [105734774294938]   (run day17_part2 "input/day17-input.txt"))
test18_1 :: Test
test18_1 = TestCase (assertEqual "Day 18, part 1" [264]               (run day18_part1 "input/day18-input.txt"))
test18_2 :: Test
test18_2 = TestCase (assertEqual "Day 18, part 2" [41,26]             (run day18_part2 "input/day18-input.txt"))
test19_1 :: Test
test19_1 = TestCase (assertEqual "Day 19, part 1" [355]               (run day19_part1 "input/day19-input.txt"))
test19_2 :: Test
test19_2 = TestCase (assertNotImplemented "Day 19, part 2" []         (run day19_part2 "input/day19-input.txt"))
test20_1 :: Test
test20_1 = TestCase (assertEqual "Day 20, part 1" [1524]              (run day20_part1 "input/day20-input.txt"))
test20_2 :: Test
test20_2 = TestCase (assertEqual "Day 20, part 2" [1033746]           (run day20_part2 "input/day20-input.txt"))
test21_1 :: Test
test21_1 = TestCase (assertNotImplemented "Day 21, part 1" []         (run day21_part1 "input/day21-input.txt"))
test21_2 :: Test
test21_2 = TestCase (assertNotImplemented "Day 21, part 2" []         (run day21_part2 "input/day21-input.txt"))
test22_1 :: Test
test22_1 = TestCase (assertEqual "Day 22, part 1" [13429191512]       (run day22_part1 "input/day22-input.txt"))
test22_2 :: Test
test22_2 = TestCase (assertNotImplemented "Day 22, part 2" []         (run day22_part2 "input/day22-input.txt"))
test23_1 :: Test
test23_1 = TestCase (assertEqual "Day 23, part 1" [1215]              (run day23_part1 "input/day23-input.txt"))
test23_2 :: Test
test23_2 = TestCase (assertNotImplemented "Day 23, part 2" []         (run day23_part2 "input/day23-input.txt"))
test24_1 :: Test
test24_1 = TestCase (assertEqual "Day 24, part 1" [53258032898766]    (run day24_part1 "input/day24-input.txt"))
test24_2 :: Test
test24_2 = TestCase (assertEqual "Day 24, part 2" []                  (run day24_part2 "input/day24-input.txt"))
test25_1 :: Test
test25_1 = TestCase (assertEqual "Day 25, part 1" [3242]              (run day25_part1 "input/day25-input.txt"))
test25_2 :: Test
test25_2 = TestCase (assertEqual "Day 25, part 2" []                  (run day25_part2 "input/day25-input.txt"))

tests :: Test
tests = TestList [
          test01_1,
          test01_2,
          test02_1,
          test02_2,
          test03_1,
          test03_2,
          test04_1,
          test04_2,
          test05_1,
          test05_2,
          test06_1,
          test06_2,
          test07_1,
          test07_2,
          test08_1,
          test08_2,
          test09_1,
          test09_2,
          test10_1,
          test10_2,
          test11_1,
          test11_2,
          test12_1,
          test12_2,
          test13_1,
          test13_2,
          test14_1,
          test14_2,
          test15_1,
          test15_2,
          test16_1,
          test16_2,
          test17_1,
          test17_2,
          test18_1,
          test18_2,
          test19_1,
          test19_2,
          test20_1,
          test20_2,
          test21_1,
          test21_2,
          test22_1,
          test22_2,
          test23_1,
          test23_2,
          test24_1,
          test24_2,
          test25_1,
          test25_2
        ]

main :: IO ()
main = do
  _ <- runTestTT tests
  return ()




