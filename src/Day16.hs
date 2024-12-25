module Day16 (
    day16_part1,
    day16_part2
    ) where

import qualified Data.Array as Array
import qualified Data.HashSet as HashSet
import Data.Graph.AStar
--import Data.Hashable (Hashable)

type Array2OfChar = Array.Array (Int, Int) Char
type Point = (Int, Int, Int)
type PointSet = HashSet.HashSet Point
type Neighborhood = Array.Array (Int, Int, Int) PointSet

createMap :: String -> Array2OfChar
createMap input =
    let a = lines input
        bottom = length a - 1
        right = length (head a) - 1
        bounds = ((0, 0), (bottom, right))
        elements = [((i, j), a !! i !! j) | i <- [0 .. bottom], j <- [0 .. right]]
    in Array.array bounds elements

-- Neighbors function: adjacent cells in a grid
neighborsOf :: Neighborhood -> Point -> PointSet
neighborsOf neighborhood p = neighborhood Array.! p

-- Turn cost function: cost of turning from direction d1 to direction d2
--    | 0 | 1 | 2 | 3
--  --+---+---+---+--
--  0 | 0 | 1 | 2 | 1
--  1 | 1 | 0 | 1 | 2
--  2 | 2 | 1 | 0 | 1
--  3 | 1 | 2 | 1 | 0
turnCost :: Int -> Int -> Int
turnCost d1 d2 = abs (((d1 - d2 + 2) `mod` 4) - 2) * 1000

-- Cost function: all moves have a cost of 1 plus the cost of turning (maybe twice)
cost :: Point -> Point -> Int
cost p1 p2 =
    let (i1, j1, d1) = p1
        (i2, j2, d2) = p2
        md = case (i2 - i1, j2 - j1) of
            (0, 1) -> 0
            (1, 0) -> 1
            (0, -1) -> 2
            (-1, 0) -> 3
            _ -> error "Invalid move!"
    in 1 + turnCost d1 md + turnCost md d2

-- Heuristic function: Manhattan distance to the goal
heuristic :: Point -> Point -> Int
heuristic (i1, j1, _) (i2, j2, _) =
    abs (i1 - i2) + abs (j1 - j2)

-- Goal predicate: have we reached the goal?
goal :: Point -> Point -> Bool
goal goalNode p =
    let (g_i, g_j, _) = goalNode
        (p_i, p_j, _) = p
    in g_i == p_i && g_j == p_j
{-
-- Valid cell predicate: is the cell a valid one?
validCell :: Array2OfChar -> (Int, Int) -> Bool
validCell area p =
    Array.inRange (Array.bounds area) p && area Array.! p /= '#'
-}
-- Adds neighbors of the given point to the neighborhood
addNeighbors :: Point -> Neighborhood -> Neighborhood
addNeighbors (i, j, d) neighborhood =
    let adjacent = [(i, j + 1), (i - 1, j), (i, j - 1), (i + 1, j)]
        neighbors = [(i', j', d') | (i', j') <- adjacent, d' <- [0..3], abs d - d' /= 2]
    in neighborhood Array.// [((i, j, d), HashSet.fromList neighbors)]

-- Builds the list of valid neighbors for each point in the grid
buildNeighborhood :: Array2OfChar -> Neighborhood
buildNeighborhood area =
    let ((_, _), (bottom, right)) = Array.bounds area
        bounds = ((0, 0, 0), (bottom, right, 3))
        elements = [((i, j, d), HashSet.empty) | i <- [0 .. bottom], j <- [0 .. right], d <- [0..3]]
        neighborhood = Array.array bounds elements
        allAccessible = [(i, j, d) | i <- [0 .. bottom], j <- [0 .. right], area Array.! (i, j) /= '#', d <- [0..3]]
    in foldr addNeighbors neighborhood allAccessible

-- Part 1
day16_part1 :: String -> IO [Int]
day16_part1 input = do
    let area = createMap input
--    print $ Array.bounds area
    let ((top, left), (bottom, right)) = Array.bounds area
    let start = (bottom - 1, left + 1, 0 :: Int)
    let end = (top + 1, right - 1, 0 :: Int)
--    print (start, end)
    let neighborhood = buildNeighborhood area
--    print neighborhood
    let path = aStar (neighborsOf neighborhood) cost (heuristic end) (goal end) start
    case path of
        Just p  -> do
--            putStrLn $ "Path found: " ++ show p
            let totalCost = cost start (head p) + foldl (\acc (p1, p2) -> acc + cost p1 p2) 0 (zip p (tail p))
            return [totalCost]
        Nothing -> error "No path found!"

-- Part 2
day16_part2 :: String -> IO [Int]
day16_part2 input = do
    return []
