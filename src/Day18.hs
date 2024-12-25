module Day18 (
    day18_part1,
    day18_part2
    ) where

import qualified Data.Array as Array
import qualified Data.HashSet as HashSet
import Data.Graph.AStar ( aStar )
import Data.List.Split ( splitOn )
--import Debug.Trace

type Array2OfChar = Array.Array (Int, Int) Char
type Point = (Int, Int)
type PointSet = HashSet.HashSet Point

right :: Int
bottom :: Int
time :: Int
---- Example map parameters
--right = 6
--bottom = 6
--time = 12
-- Puzzle map parameters
right = 70
bottom = 70
time = 1024

start :: (Int, Int)
start = (0, 0)
end :: (Int, Int)
end = (right, bottom)

createMap :: [(Int, Int)] -> Array2OfChar
createMap coords =
    let area = Array.array ((0, 0), (right, bottom)) [((i, j), '.') | i <- [0..bottom], j <- [0..right]]
    in foldr (\(x, y) area' -> area' Array.// [((y, x), '#')]) area coords

parseLine :: String -> (Int, Int)
parseLine line =
    case splitOn "," line of
        [x, y] -> (read x :: Int, read y :: Int)
        _      -> error "Invalid input format"

--printMap :: Array2OfChar -> IO ()
--printMap area = do
--    let rows = [[area Array.! (i, j) | j <- [0..right]] | i <- [0..bottom]]
--    mapM_ putStrLn rows

-- Cost function: all moves have a cost of 1 plus the cost of turning (maybe twice)
cost :: Point -> Point -> Int
cost _ _ = 1

-- Heuristic function: Manhattan distance to the goal
heuristic :: Point -> Point -> Int
heuristic (x1, y1) (x2, y2) =
    abs (x1 - x2) + abs (y1 - y2)

-- Goal predicate: have we reached the goal?
goal :: Point -> Point -> Bool
goal = (==)

-- Valid cell predicate: is the cell a valid one?
validCell :: Array2OfChar -> (Int, Int) -> Bool
validCell area p =
    Array.inRange (Array.bounds area) p && area Array.! p /= '#'

neighbors :: Array2OfChar -> Point -> PointSet
neighbors area (x, y) =
    HashSet.fromList [p | p <- [(x, y - 1), (x, y + 1), (x - 1, y), (x + 1, y)], validCell area p]

-- Part 1
day18_part1 :: String -> IO [Int]
day18_part1 input = do
    let coords = take time $ map parseLine $ lines input
--    print coords
    let area = createMap coords
--    printMap area
    let path = aStar (neighbors area) cost (heuristic end) (goal end) start
--    print path
    let totalCost = case path of
            Just p  -> length p
            Nothing -> error "No path found!"
    return [totalCost]

findFirstBlockage :: Array2OfChar -> [(Int, Int)] -> Point
findFirstBlockage _ [] = (0, 0)
findFirstBlockage area (c:coords') =
    let area' = area Array.// [(c, '#')]
        path = aStar (neighbors area') cost (heuristic end) (goal end) start
    in case path of
            Just _  -> findFirstBlockage area' coords'
            Nothing -> c

-- Part 2
day18_part2 :: String -> IO [Int]
day18_part2 input = do
    let coords = map parseLine $ lines input
--    print coords
    let area = createMap []
--    printMap area
--    print path
    let (x, y) = findFirstBlockage area coords
    return [x, y]
