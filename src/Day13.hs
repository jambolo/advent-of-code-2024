module Day13 (
    day13_part1,
    day13_part2
    ) where

import Text.Regex.TDFA
import Text.Read (readMaybe)
import Data.List (intersect)
--import Debug.Trace ( trace )

data Configuration = Configuration {
    aXY :: (Int, Int),
    bXY :: (Int, Int),
    prizeXY :: (Int, Int)
} deriving (Show)

readA :: String -> (Int, Int)
readA input =
    let aRE = "Button A: X\\+([0-9]+), Y\\+([0-9]+)"
    in case input =~ aRE :: (String, String, String, [String]) of
        (_, _, _, [a, b]) -> case (readMaybe a, readMaybe b) of
            (Just x, Just y) -> (x, y)
            _ -> error $ "Unable to parse: " ++ input
        _ -> error $ "Regex match failed: " ++ aRE ++ " -> " ++ input

readB :: String -> (Int, Int)
readB input =
    let bRE = "Button B: X\\+([0-9]+), Y\\+([0-9]+)"
    in case input =~ bRE :: (String, String, String, [String]) of
        (_, _, _, [a, b]) -> case (readMaybe a, readMaybe b) of
            (Just x, Just y) -> (x, y)
            _ -> error $ "Unable to parse: " ++ input
        _ -> error $ "Regex match failed: " ++ bRE ++ " -> " ++ input

readPrize :: String -> (Int, Int)
readPrize input =
    let prizeRE = "Prize: X=([0-9]+), Y=([0-9]+)"
    in case input =~ prizeRE :: (String, String, String, [String]) of
        (_, _, _, [a, b]) -> case (readMaybe a, readMaybe b) of
            (Just x, Just y) -> (x, y)
            _ -> error $ "Unable to parse: " ++ input
        _ -> error $ "Regex match failed: " ++ prizeRE ++ " -> " ++ input

-- Loads the configurations from the input file
loadConfigurations :: [String] -> [Configuration]
loadConfigurations input =
    next input []
    where
        next :: [String] -> [Configuration] -> [Configuration]
        next [] acc = acc
        next [_] _ = error "Invalid input"
        next [_, _] _ = error "Invalid input"
        next [_, _, _] _ = error "Invalid input"
        next (a:b:prize:_:rest) acc =
            let a' = readA a
                b' = readB b
                prize' = readPrize prize
            in next rest (Configuration a' b' prize' : acc)

-- Extended Euclidean Algorithm finds the coefficients x and y such that ax + by = gcd(a, b)
extendedEuclidean :: Int -> Int -> (Int, Int, Int)
extendedEuclidean a b =
    next (a, 1, 0) (b, 0, 1)
    where
        next :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
        next (r0, s0, t0) (r1, s1, t1) =
            let (q, r) = r0 `divMod` r1
            in if r == 0
                then (r1, s1, t1)
                else next (r1, s1, t1) (r, s0 - q * s1, t0 - q * t1)
        
-- Finds the solutions to the linear Diophantine equation, ax + by = c, using the Extended Euclidean Algorithm
--  
--  1. Compute gcd(a, b) using Euclidean Algorithm; let g = gcd(a, b)
--  2. If c % g ≠ 0:
--         Return empty list since no solution exists
--  3. Use Extended Euclidean Algorithm to find any x' and y' such that:
--         ax' + by' = g
--  4. Scale the solution by c/g to get a solution to the original problem:
--         x0 = x' * c/g
--         y0 = y' * c/g
--  5. General solution is:
--         x = x0 + k * b/g
--         y = y0 - k * a/g
--         where k is any integer
--  6. x and y must be non-negative, so the valid range for k is:
--         −x0 * g/b < k < y0 * g/a
--     Since k must be an integer, return the range of integer values of k that satisfy this inequality.
solveLinearDiophantine :: Int -> Int -> Int -> [(Int, Int)]
solveLinearDiophantine a b c =
    let (g, x', y') = extendedEuclidean a b
    in if c `mod` g == 0
        then
            let x0 = x' * (c `div` g)
                y0 = y' * (c `div` g)
                kMin = ceiling ((fromIntegral (-x0) * fromIntegral g / fromIntegral b) :: Double)
                kMax = floor ((fromIntegral y0 * fromIntegral g / fromIntegral a) :: Double)
             in [(x0 + k * b `div` g, y0 - k * a `div` g) | k <- [kMin..kMax]]
        else []

findSolutions :: [Configuration] -> [[(Int, Int)]]
findSolutions configurations =
    let xs = map (\c -> solveLinearDiophantine (fst $ aXY c) (fst $ bXY c) (fst $ prizeXY c)) configurations
        ys = map (\c -> solveLinearDiophantine (snd $ aXY c) (snd $ bXY c) (snd $ prizeXY c)) configurations
    in zipWith intersect xs ys

findLowestSolutionCost :: [(Int, Int)] -> Int
findLowestSolutionCost solution = minimum $ map (\(a, b) -> a * 3 + b) solution

-- Part 1
day13_part1 :: String -> IO [Int]
day13_part1 input = do
    let configurations = loadConfigurations $ lines input
    let solutions = findSolutions configurations
    let cost = foldr (\c acc -> if null c then acc else findLowestSolutionCost c + acc) 0 solutions
    return [cost]

bumpPrizeLocation :: Configuration -> Configuration
bumpPrizeLocation configuration =
    configuration {
        prizeXY = (fst (prizeXY configuration) + 10000000000000, snd (prizeXY configuration) + 10000000000000)
    }

-- Part 2
day13_part2 :: String -> IO [Int]
day13_part2 input = do
    let configurations = map bumpPrizeLocation $ loadConfigurations $ lines input
--    print configurations
    let solutions = map (\configuration ->
            let x  = fst (aXY configuration)
                x' = snd (aXY configuration)
                y  = fst (bXY configuration)
                y' = snd (bXY configuration)
                c  = fst (prizeXY configuration)
                c' = snd (prizeXY configuration)
                d = x * y' - x' * y
                y'cyc' = y' * c - y * c'
                xc'x'c = x * c' - x' * c
            in if d == 0 || y'cyc' `mod` d /= 0 || xc'x'c `mod` d /= 0
                then Nothing
                else Just (y'cyc' `div` d, xc'x'c `div` d)
            ) configurations
--    print solutions
    let result = sum $ map (maybe 0 (\(a, b) -> a * 3 + b)) solutions
    return [result]
