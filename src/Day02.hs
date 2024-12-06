module Day02 (
    day02_part1,
    day02_part2
    ) where

import Text.Read (readMaybe)

safeRise :: Int -> Int -> Bool
safeRise x0 x1 = x0 + 1 <= x1 && x1 <= x0 + 3

safeFall :: Int -> Int -> Bool
safeFall x0 x1 = x1 + 1 <= x0 && x0 <= x1 + 3

risingSafely :: [Int] -> Bool
risingSafely xs = all (uncurry safeRise) (zip xs (tail xs))

fallingSafely :: [Int] -> Bool
fallingSafely xs = all (uncurry safeFall) (zip xs (tail xs))

safeReport :: [Int] -> Bool
safeReport report = risingSafely report || fallingSafely report

parseLines :: [String] -> [[Int]]
parseLines = map (map readInt . words)
    where
        readInt a = case readMaybe a of
            Just x -> x
            _ -> error "Invalid input"

countSafe :: [Bool] -> Int
countSafe = length . filter id

day02_part1 :: String -> IO Int
day02_part1 input = return $ countSafe $ map safeReport $ parseLines $ lines input

removeEach :: [Int] -> [[Int]]
removeEach [] = [] -- If the list is empty, return an empty list
removeEach xs = [take i xs ++ drop (i + 1) xs | i <- [0 .. length xs - 1]]

tolerablyRising :: [Int] -> Bool
tolerablyRising report = risingSafely report || any risingSafely (removeEach report)

tolerablyFalling :: [Int] -> Bool
tolerablyFalling report = fallingSafely report || any fallingSafely (removeEach report)

tolerablySafeReport :: [Int] -> Bool
tolerablySafeReport report = tolerablyRising report || tolerablyFalling report

day02_part2 :: String -> IO Int
day02_part2 input = return $ countSafe $ map tolerablySafeReport $ parseLines $ lines input
