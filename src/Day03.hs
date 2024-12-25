module Day03 (
    day03_part1,
    day03_part2
    ) where

import Text.Regex.TDFA ((=~), getAllTextMatches)

mulRE :: String
mulRE = "mul\\(([0-9]+),([0-9]+)\\)"

parseMul :: String -> (Int, Int)
parseMul string = case string =~ mulRE :: (String, String, String, [String]) of
    (_, _, _, [a, b]) -> (read a, read b)
    _ -> error $ "Regex match failed: " ++ string

day03_part1 :: String -> IO [Int]
day03_part1 input = do
    let matches = getAllTextMatches (input =~ mulRE) :: [String]
--    print matches
    let operands = map parseMul matches
--    print operands
    let result = sum $ map (uncurry (*)) operands
    return [result]

switchedMulRE :: String
switchedMulRE = "(mul\\([0-9]+,[0-9]+\\))|(don't\\(\\))|(do\\(\\))"

extractEnabled :: [String] -> [String]
extractEnabled = snd . foldl extract (True, [])
    where
        extract (_, out) "don't()" = (False, out)
        extract (_, out) "do()" = (True, out)
        extract (enabled, out) x = (enabled, if enabled then x : out else out)

day03_part2 :: String -> IO [Int]
day03_part2 input = do
    let matches = getAllTextMatches (input =~ switchedMulRE) :: [String]
--    print matches
    let operands = map parseMul (extractEnabled matches)
--    print operands
    let result = sum $ map (uncurry (*)) operands
    return [result]
