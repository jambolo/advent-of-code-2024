module Day07 (
    day07_part1,
    day07_part2
    ) where

import Data.List.Split (splitOn)

parseLines :: [String] -> [(Int, [Int])]
parseLines = map parseLine
    where
        parseLine s =
            case splitOn ":" s of
                [result, operands] -> (read result, map read (words operands))
                _ -> error "Invalid input"

testRecursive :: Int -> Int -> [Int] -> Bool
testRecursive result acc operands =
    case operands of
        [] -> acc == result
        _ -> testRecursive result (acc + head operands) (tail operands) ||
             testRecursive result (acc * head operands) (tail operands)

testComputation :: (Int, [Int]) -> Bool
testComputation (result, operands) = testRecursive result (head operands) (tail operands)

nextPowerOf10 :: Int -> Int
nextPowerOf10 n = head [10^x | x <- [1..] :: [Int], 10^x > n]

concatenate :: Int -> Int -> Int
concatenate a b = a * nextPowerOf10 b + b

testRecursive2 :: Int -> Int -> [Int] -> Bool
testRecursive2 result acc operands =
    case operands of
        [] -> acc == result
        (x:xs) -> acc <= result &&
            (testRecursive2 result (acc + x) xs ||
            testRecursive2 result (acc * x) xs ||
            testRecursive2 result (concatenate acc x) xs)

testComputation2 :: (Int, [Int]) -> Bool
testComputation2 (result, operands) = testRecursive2 result (head operands) (tail operands)

day07_part1 :: String -> IO Int
day07_part1 input = do
    let equations = parseLines $ lines input
    let validEquations = filter testComputation equations
    return $ sum $ map fst validEquations

day07_part2 :: String -> IO Int
day07_part2 input = do
    let equations = parseLines $ lines input
    let validEquations = filter testComputation2 equations
    return $ sum $ map fst validEquations
