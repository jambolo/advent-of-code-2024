module Day22 (
    day22_part1,
    day22_part2
    ) where

--import Debug.Trace ( trace )
import Data.Bits ( xor )
import qualified Data.Array as Array

type PricesBySeqAndMonkey = Array.Array (Int, Int) Int

rng :: Int -> Int
rng x =
    let y0 = x `xor` (x * 64) `mod` 16777216
        y1 = y0 `xor` (y0 `div` 32) `mod` 16777216
        y2 = y1 `xor` (y1 * 2048) `mod` 16777216
    in y2

-- Part 1
day22_part1 :: String -> IO Int
day22_part1 input = do
    let s0 = map read (lines input) :: [Int]
--    print s0
    let s2000 = map (\s -> iterate rng s !! 2000) s0
--    print $ zip s0 s2000
    let result = sum s2000
    return result

-- Number of possible sequences
nSeq :: Int
nSeq = 19^(4 :: Integer)

-- Number of secret numbers
nNumbers :: Int
nNumbers = 2001

convertSequenceToIndex :: [Int] -> Int
convertSequenceToIndex = foldl (\acc x -> acc * 19 + (x + 9)) 0

--convertIndexToSequence :: Int -> [Int]
--convertIndexToSequence i = fst $ iterate (\acc -> (let (q, r) = snd acc `divMod` 19 in ((r - 9) : fst acc, q))) ([], i) !! 4

-- Get the specified element and the previous 3 elements
get4 :: [a] -> Int -> [a]
get4 xs i = take 4 (drop (i - 3) xs)

-- Creates a PricesBySeqAndMonkey initialized to 0
initializePricesBySeqAndMonkey :: Int -> PricesBySeqAndMonkey
initializePricesBySeqAndMonkey m = Array.array ((0, 0), (nSeq - 1, m - 1)) [((i, j), 0) | i <- [0..nSeq - 1], j <- [0..m - 1]]

-- Builds the prices-by-sequence map
buildPricesBySeqAndMonkey :: [[Int]] -> [[Int]] -> PricesBySeqAndMonkey
buildPricesBySeqAndMonkey differences prices =
    let nMonkeys = length prices
        pricesBySeqAndMonkey = initializePricesBySeqAndMonkey nMonkeys
    in foldl (\acc m ->
            let differences' = 0 : differences !! m
                prices' = prices !! m
            in foldl (\acc' i ->
                let index = convertSequenceToIndex $ get4 differences' i
                    p = prices' !! i
                in acc' Array.// [((index, m), p)]
                ) acc [4..nNumbers - 1] -- 4 - 2000
            ) pricesBySeqAndMonkey [0..nMonkeys - 1]

rowSum :: PricesBySeqAndMonkey -> Int -> Int
rowSum array r = sum [array Array.! (r, c) | c <- [0..snd (snd (Array.bounds array))]]

-- Part 2
day22_part2 :: String -> IO Int
day22_part2 input = do
    let s0 = map read (lines input) :: [Int]
--    print s0
    let s2000 = map (take nNumbers . iterate rng) s0 -- for each monkey
--    print s2000
    let prices = map (map (`mod` 10)) s2000 -- 0 - 2000 for each monkey
--    print prices
    let differences = map (\p -> zipWith (-) (tail p) p) prices -- 1 - 2000 for each monkey
--    print differences
    let pricesBySeqAndMonkey = buildPricesBySeqAndMonkey differences prices -- prices for 0 - nMonkeys-1 for 0 - 19^4-1 sequences
--    print pricesBySeqAndMonkey
--    let nonZeroElements = map (\((i, m), p) -> ((convertIndexToSequence i, m), p)) $ filter (\((_, _), v) -> v /= 0) (Array.assocs pricesBySeqAndMonkey)
--    print nonZeroElements
    let result = foldr (\i acc ->
                    let s = rowSum pricesBySeqAndMonkey i
                    in if s > snd acc then (i, s) else acc
                    ) (0, 0) [0..nSeq - 1]
--    print $ convertIndexToSequence $ fst result
    return $ snd result
