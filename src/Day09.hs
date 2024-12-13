module Day09 (
    day09_part1,
    day09_part2
    ) where

import Data.Char (digitToInt)

-- Parses a string of digits into a list of integers
parseDigits :: String -> [Int]
parseDigits = map digitToInt . filter (/= '\n')

-- Builds a sector map from a list of digits
buildSectorMap :: [Int] -> [Int]
buildSectorMap =
    reverse . next [] True 0
    where
        next :: [Int] -> Bool -> Int -> [Int] -> [Int]
        next acc _ _ [] = acc
        next acc f i (x:xs) =
            if f
                then next (replicate x    i ++ acc) (not f) (i + 1) xs
                else next (replicate x (-1) ++ acc) (not f) i xs

defrag :: [Int] -> [Int]
defrag sectors =
    let reversed = dropWhile (< 0) $ reverse sectors
    in next [] sectors reversed
    where
        next :: [Int] -> [Int] -> [Int] -> [Int]
        next acc _ [] = reverse acc
        next acc [] _ = reverse acc
        next acc (x:xs) r =
            if x >= 0
                then next (x:acc) xs (init r)
                else let (r0:rs) = r
                    in next (r0:acc) xs (dropWhile (< 0) (init rs))

checksum :: [Int] -> Int
checksum = sum . filter (>= 0) . zipWith (\i x -> if x >= 0 then i * x else x) [0..]

day09_part1 :: String -> IO Int
day09_part1 input = do
    let digits = parseDigits input
    let sectors = buildSectorMap digits
    let defragged = defrag sectors
    return $ checksum defragged


day09_part2 :: String -> IO Int
day09_part2 input = do
    return 0
