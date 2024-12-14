module Day09 (
    day09_part1,
    day09_part2
    ) where

import Data.Char (digitToInt)

-- Parses a string of digits into a list of integers
parseDigits :: String -> [Int]
parseDigits = map digitToInt . filter (/= '\n')

-- Builds a sector map from a list of digits
buildSectorList :: [Int] -> [Int]
buildSectorList =
    reverse . next [] True 0
    where
        next :: [Int] -> Bool -> Int -> [Int] -> [Int]
        next acc _ _ [] = acc
        next acc f i (x:xs) =
            if f
                then next (replicate x    i ++ acc) (not f) (i + 1) xs
                else next (replicate x (-1) ++ acc) (not f) i xs

-- "Defragments" a sector map according to the rules in part 1
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

-- Calculates the checksum of a sector map
checksum :: [Int] -> Int
checksum = sum . filter (>= 0) . zipWith (\i x -> if x >= 0 then i * x else x) [0..]

day09_part1 :: String -> IO Int
day09_part1 input = do
    let digits = parseDigits input
    let sectors = buildSectorList digits
    let defragged = defrag sectors
    return $ checksum defragged

data Bucket = Bucket {
    size :: Int,
    value :: Int
} deriving (Show)

-- Builds the list of buckets from a list of digits
buildBucketList :: [Int] -> [Bucket]
buildBucketList =
    reverse . next [] True 0
    where
        next :: [Bucket] -> Bool -> Int -> [Int] -> [Bucket]
        next acc _ _ [] = acc
        next acc f i (x:xs) =
            if f
                then next (Bucket x i : acc) (not f) (i + 1) xs
                else next (Bucket x (-1) : acc) (not f) i xs
{-
-- Prints a list of buckets on a single line. For each bucket, the value is printed size times. If the value is -1, printed character is '.'
printBuckets :: [Bucket] -> IO ()
printBuckets buckets = do
    let chars = concatMap (\b -> replicate (size b) (if value b < 0 then '.' else intToDigit (value b))) buckets
    putStrLn chars
-}
-- Splits a bucket into two smaller buckets. The first bucket is the given bucket and the second has the leftover size and a value of -1
fillEmptyBucket :: Bucket -> Bucket -> [Bucket]
fillEmptyBucket empty r =
    [r, Bucket (size empty - size r) (-1)]

-- Returns an empty version of a bucket
emptyBucket :: Bucket -> Bucket
emptyBucket b = Bucket (size b) (-1)

-- Defragments a list of buckets according to the rules in part 2
defrag2 :: [Bucket] -> [Bucket]
defrag2 buckets =
    let reversed = dropWhile (\b -> value b < 0) $ reverse buckets
    in next buckets reversed
    where
        next :: [Bucket] -> [Bucket] -> [Bucket]
        next acc [] = acc
        next acc (r:rs) =
            let (left, right) = break (\b -> value b == value r) acc
                rs' = dropWhile (\b -> value b < 0) rs
            in case break (\b -> value b < 0 && size b >= size r) left of
                (_, []) -> next acc rs'
                (before, empty:after) ->
                    let current = head right
                        acc' = before ++ fillEmptyBucket empty r ++ after ++ [emptyBucket current] ++ tail right
                    in next acc' rs'

checksum2 :: [Bucket] -> Int
checksum2 buckets =
    snd $ foldl (\(i, acc) b ->
            let size' = size b
                value' = if value b > 0 then value b else 0
                sumOfIndexes = size' * (size' - 1) `div` 2 + i * size'
            in (i + size', value' * sumOfIndexes + acc)
        ) (0, 0) buckets

day09_part2 :: String -> IO Int
day09_part2 input = do
    let digits = parseDigits input
    let buckets = buildBucketList digits
    print $ length buckets
--    printBuckets buckets
    let defragged = defrag2 buckets
    print $ length defragged
--    printBuckets defragged
    return $ checksum2 defragged
