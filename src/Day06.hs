module Day06 (
    day06_part1,
    day06_part2
    ) where

import qualified Data.Array as Array
import qualified Data.Map as Map
import qualified Data.Set as Set

-- Visited is a dictionary of locations and their facings
type Array2OfChar = Array.Array (Int, Int) Char
type FacingSet = Set.Set Char
type VisitedMap = Map.Map (Int, Int) FacingSet

createMap :: String -> Array2OfChar
createMap input =
    let a = lines input
        bottom = length a - 1
        right = length (head a) - 1
        bounds = ((0, 0), (bottom, right))
        elements = [((i, j), a !! i !! j) | i <- [0 .. bottom], j <- [0 .. right]]
    in Array.array bounds elements

-- Finds the location of '^', '<', 'v', or '>' in area
findStart :: Array2OfChar -> Maybe ((Int, Int), Char)
findStart area = 
    foldr (\idx acc ->
        let symbol = area Array.! idx
        in if symbol `elem` ">^<v" then Just (idx, symbol) else acc
        ) Nothing (Array.range (Array.bounds area))

rightTurn :: Char -> Char
rightTurn facing = case facing of
    '>' -> 'v'
    '^' -> '>'
    '<' -> '^'
    'v' -> '<'
    _ -> error "Invalid facing"

step :: (Int, Int) -> Char -> (Int, Int)
step (i, j) facing = case facing of
    '>' -> (i    , j + 1)
    '^' -> (i - 1, j)
    '<' -> (i    , j - 1)
    'v' -> (i + 1, j)
    _ -> error "Invalid facing"
    

travel :: Array2OfChar -> (Int, Int) -> Char -> VisitedMap
travel area start initialFacing =
    go start initialFacing Map.empty
    where
        go :: (Int, Int) -> Char -> VisitedMap -> VisitedMap
        go location facing visited =
            let visited' = Map.insertWith Set.union location (Set.singleton facing) visited
                bounds = Array.bounds area
                location' = step location facing
            in if Array.inRange bounds location'
                then if area Array.! location' == '#'
                    then let facing' = rightTurn facing
                        in go location facing' visited'
                    else go location' facing visited'
                else visited'

day06_part1 :: String -> IO [Int]
day06_part1 input = do
    let area = createMap input
        (start, facing) = case findStart area of
            Just (s, f) -> (s, f)
            _ -> error "No starting point found"
    let visited = travel area start facing
    return [length visited]

findCycle :: Array2OfChar -> (Int, Int) -> Char -> Bool
findCycle area start initialFacing = go start initialFacing Map.empty
    where
        go :: (Int, Int) -> Char -> VisitedMap -> Bool
        go location facing visited
            | Map.member location visited && Set.member facing (visited Map.! location) = True
            | otherwise =
                let visited' = Map.insertWith Set.union location (Set.singleton facing) visited
                    bounds = Array.bounds area
                    location' = step location facing
                in Array.inRange bounds location' && -- move to next cell or turn
                    if area Array.! location' == '#'
                        then go location (rightTurn facing) visited' -- don't move, just turn right
                        else go location' facing visited' -- move to next cell

day06_part2 :: String -> IO [Int]
day06_part2 input = do
    let area = createMap input
        (start, facing) = case findStart area of
            Just (s, f) -> (s, f)
            _ -> error "No starting point found"
    let visited = travel area start facing
    let result = length $ filter id $ [findCycle (area Array.// [(k, '#')]) start facing | k <- Map.keys visited]
    return [result]
