module Day22 (
    day22_part1,
    day22_part2
    ) where

--import Debug.Trace ( trace )
import Data.Bits ( xor )
import Answer (Answer(..))

rng :: Int -> Int
rng x =
    let y0 = x `xor` (x * 64) `mod` 16777216
        y1 = y0 `xor` (y0 `div` 32) `mod` 16777216
        y2 = y1 `xor` (y1 * 2048) `mod` 16777216
    in y2

-- Part 1
day22_part1 :: String -> IO Answer
day22_part1 input = do
    let s0 = map read (lines input) :: [Int]
--    print s0
    let s2000 = map (\s -> iterate rng s !! 2000) s0
--    print $ zip s0 s2000
    let result = sum s2000
    return (Ints [result])

-- Part 2
day22_part2 :: String -> IO Answer
day22_part2 _input = do
    return (Ints [])
