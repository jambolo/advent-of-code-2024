module Day11 (
    day11_part1,
    day11_part2
    ) where

import qualified Data.Map as Map

type StoneMap = Map.Map Int Int

loadStones :: String -> [Int]
loadStones input = map read $ words input

-- Returns True if a number has an even number of digits
evenDigits :: Int -> Bool
evenDigits n = even $ length $ show n

blink :: [Int] -> [Int]
blink stones =
    next stones []
    where
        next [] acc = reverse acc
        {-       
            1. If the stone is engraved with the number 0, it is replaced by a stone engraved with the number 1.
            2. If the stone is engraved with a number that has an even number of digits, it is replaced by two stones.
                The left half of the digits are engraved on the new left stone, and the right half of the digits are
                engraved on the new right stone. (The new numbers don't keep extra leading zeroes: 1000 would become
                stones 10 and 0.)
            3. If none of the other rules apply, the stone is replaced by a new stone; the old stone's number
                multiplied by 2024 is engraved on the new stone.
        -}
        next (x:xs) acc
            | x == 0 = next xs (1:acc)
            | evenDigits x = next xs (right:left:acc) -- left and right are reversed because we are using a stack
            | otherwise = next xs (x * 2024:acc)
            where
                s = show x
                left = read $ take (length s `div` 2) s
                right = read $ drop (length s `div` 2) s

-- Part 1
day11_part1 :: String -> IO Int
day11_part1 input = do
    let stones = loadStones input
--    print stones
    let result = foldr (\_ acc -> blink acc) stones [1..25]
--    print result
    return $ length result

blink2 :: StoneMap -> StoneMap
blink2 stones =
    next (Map.toList stones) Map.empty
    where
        next :: [(Int, Int)] -> StoneMap -> StoneMap
        next [] acc = acc
        {-       
            1. If the stone is engraved with the number 0, it is replaced by a stone engraved with the number 1.
            2. If the stone is engraved with a number that has an even number of digits, it is replaced by two stones.
                The left half of the digits are engraved on the new left stone, and the right half of the digits are
                engraved on the new right stone. (The new numbers don't keep extra leading zeroes: 1000 would become
                stones 10 and 0.)
            3. If none of the other rules apply, the stone is replaced by a new stone; the old stone's number
                multiplied by 2024 is engraved on the new stone.
        -}
        next ((k, v):xs) acc
            | k == 0 = next xs (Map.insertWith (+) 1 v acc)
            | evenDigits k = next xs (Map.insertWith (+) left v (Map.insertWith (+) right v acc))
            | otherwise = next xs (Map.insertWith (+) (k * 2024) v acc)
            where
                s = show k
                left = read $ take (length s `div` 2) s
                right = read $ drop (length s `div` 2) s


-- Part 2
day11_part2 :: String -> IO Int
day11_part2 input = do
    let stones = Map.fromListWith (+) $ zip (loadStones input) (repeat 1)
--    print stones
    let result = foldr (\_ acc -> blink2 acc) stones [1..75]
--    print result
    let numberOfStones = Map.foldr (+) 0 result
    return numberOfStones
