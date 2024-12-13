module Day08 (
    day08_part1,
    day08_part2
    ) where

import qualified Data.Array as Array
import qualified Data.Map as Map
import qualified Data.Set as Set

type Array2OfChar = Array.Array (Int, Int) Char
type AntennaMap = Map.Map Char [(Int, Int)]
type NodeSet = Set.Set (Int, Int)

createMap :: [[Char]] -> Array2OfChar
createMap a =
    let nRows = length a
        nCols = length (head a)
        bounds = ((0, 0), (nRows - 1, nCols - 1))
        elements = [((i, j), a !! i !! j) | i <- [0 .. nRows - 1], j <- [0 .. nCols - 1]]
    in Array.array bounds elements

{-
printMap :: Array2OfChar -> IO ()
printMap area = do
    let ((minRow, minCol), (maxRow, maxCol)) = Array.bounds area
    mapM_ (\i -> do
            mapM_ (\j -> putChar (area Array.! (i, j))) [minCol .. maxCol]
            putStrLn ""
        ) [minRow .. maxRow]
-}

findAntennas :: Array2OfChar -> AntennaMap
findAntennas area =
    let bounds = Array.bounds area
    in
        foldr (\idx acc ->
            let symbol = area Array.! idx
            in
                if symbol /= '.'
                then Map.insertWith (++) symbol [idx] acc
                else acc
        ) Map.empty (Array.range bounds)

-- Get all permutations of node location pairs
permutations :: [(Int, Int)] -> [((Int, Int), (Int, Int))]
permutations xs = [(x, y) | x <- xs, y <- xs, x /= y]

antinode :: (Int, Int) -> (Int, Int) -> (Int, Int)
antinode (x1, y1) (x2, y2) =
    (x1 + (x1 - x2), y1 + (y1 - y2))

findAntinodes :: Array2OfChar -> AntennaMap -> NodeSet
findAntinodes area antennas =
    let bounds = Array.bounds area
    in
        foldr (\p acc ->
            let a = uncurry antinode p
            in
                if Array.inRange bounds a
                then Set.insert a acc
                else acc
        ) Set.empty (concatMap permutations (Map.elems antennas))

day08_part1 :: String -> IO Int
day08_part1 input = do
    let area = createMap (lines input)
    let antennas = findAntennas area
    let antinodes = findAntinodes area antennas
    return $ length antinodes

harmonic :: Array2OfChar -> ((Int, Int), (Int, Int)) -> [(Int, Int)]
harmonic area ((x1, y1), (x2, y2)) =
    let dx = x2 - x1
        dy = y2 - y1
        bounds = Array.bounds area
        next (x, y) acc
            | Array.inRange bounds (x, y) = next (x + dx, y + dy) ((x, y) : acc)
            | otherwise = acc
    in
        next (x2, y2) []

findHarmonics :: Array2OfChar -> AntennaMap -> NodeSet
findHarmonics area antennas =
    foldr (\p acc ->
        let hs = harmonic area p
        in
            Set.union (Set.fromList hs) acc
    ) Set.empty (concatMap permutations (Map.elems antennas))

day08_part2 :: String -> IO Int
day08_part2 input = do
    let area = createMap (lines input)
    let antennas = findAntennas area
    let harmonics = findHarmonics area antennas
    return $ length harmonics
