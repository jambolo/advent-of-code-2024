module Day03 (
    day03_part1,
    day03_part2
    ) where

import Text.Regex.TDFA
import Text.Read (readMaybe)

mulRE :: String
mulRE = "mul\\(([0-9]+),([0-9]+)\\)"

executeMul :: (Int, Int) -> Int
executeMul (a, b) = a * b

parseMul :: String -> (Int, Int)
parseMul string =
    case string =~ mulRE :: (String, String, String, [String]) of
        (_, _, _, [a, b]) -> case (readMaybe a, readMaybe b) of
            (Just x, Just y) -> (x, y)
            _ -> error $ "Unable to parse: " ++ string
        _ -> error $ "Regex match failed: " ++ string

day03_part1 :: String -> IO Int
day03_part1 input = do
    let matches = getAllTextMatches (input =~ mulRE) :: [String]
    return $ sum $ map executeMul $ map parseMul matches

switchedMulRE :: String
switchedMulRE = "(mul\\([0-9]+,[0-9]+\\))|(don't\\(\\))|(do\\(\\))"

extractEnabled :: [String] -> [String]
extractEnabled = next True []
    where
        next _ out [] = out
        next enabled out (x:xs)
            | x == "don't()" = next False out xs 
            | x == "do()" = next True out xs
            | enabled = next enabled (out ++ [x]) xs
            | otherwise = next enabled out xs

day03_part2 :: String -> IO Int
day03_part2 input = do
    let matches = getAllTextMatches (input =~ switchedMulRE) :: [String]
    return $ sum $ map executeMul$ map parseMul $ extractEnabled matches
