module Day07 (
    day07_part1,
    day07_part2
    ) where

import Data.List.Split (splitOn)

parseLines :: [String] -> [(Int, [Int])]
parseLines = map parseLine
    where
        parseLine :: String -> (Int, [Int])
        parseLine s = case splitOn ":" s of
            [result, operands] -> (read result, map read (words operands))
            _ -> error "Invalid input"

testComputation :: (Int, [Int]) -> Bool
testComputation (result, operands) =
    next (tail operands) (head operands)
    where
        next :: [Int] -> Int -> Bool
        next [] acc = acc == result
        next (x:xs) acc = acc <= result && (next xs (acc + x) || next xs (acc * x))

nextPowerOf10 :: Int -> Int
nextPowerOf10 n = head [10^x | x <- [1..] :: [Int], 10^x > n]

concatenate :: Int -> Int -> Int
concatenate a b = a * nextPowerOf10 b + b


testComputation2 :: (Int, [Int]) -> Bool
testComputation2 (_, []) = False
testComputation2 (result, operands) =
    next (tail operands) (head operands)
    where
        next :: [Int] -> Int -> Bool
        next [] acc = acc == result
        next (x:xs) acc = acc <= result &&
                    (next xs (acc + x) ||
                    next xs (acc * x) ||
                    next xs (concatenate acc x))


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
