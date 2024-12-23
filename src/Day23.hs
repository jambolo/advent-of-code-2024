module Day23 (
    day23_part1,
    day23_part2
    ) where

import Data.List.Split (splitOn)
import qualified Data.Set as Set
--import Debug.Trace ( trace, traceShow )

type Node = String
type Edge = (String, String)
type VisitedSet = Set.Set Node
type CycleSet = Set.Set [Node]

cycleLength :: Int
cycleLength = 3

findAdjacents :: Node -> [Node] -> [Edge] -> [Node]
findAdjacents node nodes edges =
    [if x == node then y else x | (x, y) <- edges, (x == node && y `elem` nodes) || (y == node && x `elem` nodes)]

findCyclesFrom :: Node -> [Node] -> [Edge] -> [[Node]]
findCyclesFrom start nodes edges =
    next 0 start Set.empty []
        where
            next :: Int -> Node -> VisitedSet -> [Node] -> [[Node]]
            next depth node visited path
                | node == start && depth == cycleLength = [path] -- Reached start on the last edge, add the path
                | node == start && depth > 0 = [] -- Reached start but not on the last edge, so ignore
                | depth == cycleLength = [] -- On the last edge, but it's not the start, so ignore
                | otherwise = -- Continue to the next nodes
                    let visited' = Set.insert node visited
                        path' = node : path
                        adjacents = filter (\n -> n == start || not (Set.member n visited')) $ findAdjacents node nodes edges
                    in 
                        foldr (\n cycles -> next (depth + 1) n visited' path' ++ cycles) [] adjacents

findCycles :: [Node] -> [Edge] -> [[Node]]
findCycles [] _ = []
findCycles [_] _ = []
findCycles [_, _] _ = []
findCycles nodes edges =
    let node = head nodes
        nodes' = tail nodes
    in findCyclesFrom node nodes edges ++ findCycles nodes' edges

dedupCycles :: [[Node]] -> CycleSet
dedupCycles cycles =
    Set.fromList $ map Set.toList $ Set.toList $ Set.fromList $ map Set.fromList cycles

-- Returns the number of cycles containing a node starting with 'T'
countCyclesWithANodeStartingWithT :: CycleSet -> Int
countCyclesWithANodeStartingWithT cycles =
    length $ filter (any (\n -> head n == 't')) $ Set.toList cycles

-- Part 1

day23_part1 :: String -> IO Int
day23_part1 input = do
    let edges = map (\e -> (
                case  splitOn "-" e of
                    [x, y] -> (x, y)
                    _ -> error "Invalid edge format"
                    )
                ) $ lines input
--    print (length edges, edges)
    let nodes = Set.toList $ Set.fromList $ concatMap (\(x, y) -> [x, y]) edges
--    print (length nodes, nodes)
    let cycles = dedupCycles $ findCycles nodes edges
--    print (length cycles, cycles)
    let result = countCyclesWithANodeStartingWithT cycles
    return result

-- Part 2
day23_part2 :: String -> IO Int
day23_part2 input = do
    return 0
