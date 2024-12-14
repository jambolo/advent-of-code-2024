module Day12 (
    day12_part1,
    day12_part2
    ) where

import qualified Data.Set as Set
import qualified Data.Array as Array

type Region = Set.Set (Int, Int)
type Array2OfChar = Array.Array (Int, Int) Char

-- Load the input into an Area data structure
loadArea :: String -> Array2OfChar
loadArea input =
    let rows = lines input
        width = length (head rows)
        height = length rows
    in Array.array ((0, 0), (width - 1, height - 1)) [((i, j), (rows !! i) !! j) | i <- [0..height - 1], j <- [0..width - 1]]

-- Print the map of the area
printMap :: Array2OfChar -> IO ()
printMap area = do
    let ((minX, minY), (maxX, maxY)) = Array.bounds area
    mapM_ (\i -> do
        mapM_ (\j -> putChar (area Array.! (i, j))) [minX..maxX]
        putChar '\n'
        ) [minY..maxY]

-- Returns true if the given coordinate is in any region
inAnyRegion :: [Region] -> (Int, Int) -> Bool
inAnyRegion regions coord = any (Set.member coord) regions

-- Returns the neighbors of the given coordinate that have the same value as the given plant

newNeighborsOf :: (Int, Int) -> Region -> Array2OfChar -> [(Int, Int)]
newNeighborsOf (i, j) region area =
    let plant = area Array.! (i, j)
        bounds = Array.bounds area
        neighbors = [(i, j + 1), (i - 1, j), (i, j - 1), (i + 1, j)]
    in filter (\(i', j') -> 
        Array.inRange bounds (i', j') &&
        area Array.! (i', j') == plant &&
        not (Set.member (i', j') region)
        ) neighbors
    
-- Builds a region starting from the given coordinate using flood fill algorithm
buildRegion :: Array2OfChar -> (Int, Int) -> Region
buildRegion area start =
    next (Set.fromList [start]) [start]
    where
        next :: Region -> [(Int, Int)] -> Region
        next region [] = region
        next region (p:ps) =
            let neighbors = newNeighborsOf p region area
                region' = foldr Set.insert region neighbors
            in next region' (neighbors ++ ps)

-- Process the map of the area extracting a list of regions. Each region is a set of coordinates represented by integer tuples.
buildRegions :: Array2OfChar -> [Region]
buildRegions area =
    foldr (\p acc ->
        if not (inAnyRegion acc p)
        then buildRegion area p : acc
        else acc
        ) [] (Array.range (Array.bounds area))

sizeOfRegion :: Region -> Int
sizeOfRegion = Set.size

neighborsInRegion :: Region -> (Int, Int) -> [(Int, Int)]
neighborsInRegion region (i, j) =
    let neighbors = [(i, j + 1), (i - 1, j), (i, j - 1), (i + 1, j)]
    in filter (`Set.member` region) neighbors

perimeterOfRegion :: Region -> Int
perimeterOfRegion region =
    foldr (\p acc -> (4 - length (neighborsInRegion region p)) + acc) 0 region  

-- Part 1
day12_part1 :: String -> IO Int
day12_part1 input = do
    let area = loadArea input
--    print $ width area
--    print $ height area
--    printMap area
    let regions = buildRegions area
--    print regions
    let totalPrice = foldr (\r acc -> perimeterOfRegion r * sizeOfRegion r + acc) 0 regions
    return totalPrice

-- Part 2
day12_part2 :: String -> IO Int
day12_part2 input = do
    return 0
