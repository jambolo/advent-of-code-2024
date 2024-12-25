module Day10 (
    day10_part1,
    day10_part2
    ) where

import qualified Data.Array as Array
import Data.Char (digitToInt)
import qualified Data.Set as Set

type Array2OfInts = Array.Array (Int, Int) Int
type LocationSet = Set.Set (Int, Int)

-- Create the map from the input lines
createMap :: String -> Array2OfInts
createMap input =
    let a = lines input
        bottom = length a - 1
        right = length (head a) - 1
        bounds = ((0, 0), (bottom, right))
        elements = [((i, j), digitToInt (a !! i !! j)) | i <- [0 .. bottom], j <- [0 .. right]]
    in Array.array bounds elements
{-
-- Print the map
printMap :: Array2OfInts -> IO ()
printMap area = do
    let ((minRow, minCol), (maxRow, maxCol)) = Array.bounds area
    mapM_ (\i -> do
            mapM_ (\j -> putChar (intToDigit (area Array.! (i, j))) ) [minCol .. maxCol]
            putStrLn ""
        ) [minRow .. maxRow]

-}
-- Return a list of the indexes of all elements on the map that are 0
findTrailheads :: Array2OfInts -> [(Int, Int)]
findTrailheads area =
    foldr (\i acc -> if area Array.! i == 0 then i : acc else acc) [] (Array.range (Array.bounds area))

-- Returns the score for a trailhead
scoreTrailhead :: Array2OfInts -> (Int, Int) -> Int
scoreTrailhead area location =
    length $ findSummits location
    where
        findSummits :: (Int, Int) -> LocationSet
        findSummits p =
            if area Array.! p == 9
            then Set.singleton p
            else
                let bounds = Array.bounds area
                    (i, j) = p
                    h = area Array.! p
                    branches = [(i + 0, j + 1), (i - 1, j + 0), (i + 0, j - 1), (i + 1, j + 0)]
                in
                    foldr (\b acc -> if Array.inRange bounds b && ((area Array.! b) == h + 1)
                        then Set.union acc (findSummits b)
                        else acc
                    ) Set.empty branches

-- Returns each score for all trailheads
scoreTrailheads :: Array2OfInts -> [(Int, Int)] -> [Int]
scoreTrailheads area = map (scoreTrailhead area)

-- Part 1
day10_part1 :: String -> IO [Int]
day10_part1 input = do
    let area = createMap input
--    printMap area
    let trailheads = findTrailheads area
--    print trailheads
    let scores = scoreTrailheads area trailheads
--    print scores
    let result = sum scores
    return [result]

-- Returns the rating for a trailhead
rateTrailhead :: Array2OfInts -> (Int, Int) -> Int
rateTrailhead area =
    countPaths
    where
        countPaths :: (Int, Int) -> Int
        countPaths p =
            if area Array.! p == 9
            then 1
            else
                let bounds = Array.bounds area
                    (i, j) = p
                    h = area Array.! p
                    branches = [(i + 0, j + 1), (i - 1, j + 0), (i + 0, j - 1), (i + 1, j + 0)]
                in
                    sum $
                    map (\b -> if Array.inRange bounds b && ((area Array.! b) == h + 1)
                        then countPaths b
                        else 0
                    ) branches

-- Returns each rating for all trailheads
rateTrailheads :: Array2OfInts -> [(Int, Int)] -> [Int]
rateTrailheads area = map $ rateTrailhead area

-- Part 2
day10_part2 :: String -> IO [Int]
day10_part2 input = do
    let area = createMap input
--    printMap area
    let trailheads = findTrailheads area
--    print trailheads
    let ratings = rateTrailheads area trailheads
--    print ratings
    let result = sum ratings
    return [result]
