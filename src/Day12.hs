module Day12 (
    day12_part1,
    day12_part2
    ) where

import qualified Data.Set as Set
import qualified Data.Array as Array
import Data.List (sortBy)

type Region = Set.Set (Int, Int)
type Array2OfChar = Array.Array (Int, Int) Char
type Fence = (Int, (Int, Int))

-- Load the input into an Area data structure
loadArea :: String -> Array2OfChar
loadArea input = Array.array bounds [((i, j), (rows !! i) !! j) | i <- [0..bottom], j <- [0..right]]
    where
        rows = lines input
        right = length (head rows) - 1
        bottom = length rows - 1
        bounds = ((0, 0), (bottom, right))
{-
-- Print the map of the area
printMap :: Array2OfChar -> IO ()
printMap area = do
    let ((top, left), (bottom, right)) = Array.bounds area
    mapM_ (\i -> do
        mapM_ (\j -> putChar (area Array.! (i, j))) [left..right]
        putChar '\n'
        ) [top..bottom]
-}
-- Returns the 4 cells adjacent to the given coordinate
adjacentCells :: (Int, Int) -> [(Int, Int)]
adjacentCells (i, j) = [(i, j + 1), (i - 1, j), (i, j - 1), (i + 1, j)]

-- Returns true if the given coordinate is in any region
inAnyRegion :: [Region] -> (Int, Int) -> Bool
inAnyRegion regions coord = any (Set.member coord) regions

-- Returns the neighbors of the given coordinate that have the same value as the given plant and are not in the given region
newNeighbors :: (Int, Int) -> Region -> Array2OfChar -> [(Int, Int)]
newNeighbors p region area = filter (\p' -> area Array.! p' == plant && not (Set.member p' region)) adjacentInBounds
    where
        plant = area Array.! p
        adjacentInBounds = filter (Array.inRange (Array.bounds area)) (adjacentCells p)
    
-- Builds a region starting from the given coordinate using flood fill algorithm
buildRegion :: Array2OfChar -> (Int, Int) -> Region
buildRegion area start = go (Set.fromList [start]) [start]
    where
        go :: Region -> [(Int, Int)] -> Region
        go region [] = region
        go region (p:ps) =
            let neighbors = newNeighbors p region area
                region' = foldr Set.insert region neighbors
            in go region' (neighbors ++ ps)

-- Process the map of the area extracting a list of regions. Each region is a set of coordinates represented by integer tuples.
buildRegions :: Array2OfChar -> [Region]
buildRegions area = foldr (\p acc ->
    if not (inAnyRegion acc p)
    then buildRegion area p : acc
    else acc
    ) [] (Array.range (Array.bounds area))

-- Returns the size of the given region
sizeOfRegion :: Region -> Int
sizeOfRegion = Set.size

-- Returns the neighbors of the given coordinate that are in the given region
neighborsInRegion :: Region -> (Int, Int) -> [(Int, Int)]
neighborsInRegion region p = filter (`Set.member` region) (adjacentCells p)

-- Returns the perimeter of the given region
perimeterOfRegion :: Region -> Int
perimeterOfRegion region = foldr (\p acc -> (4 - length (neighborsInRegion region p)) + acc) 0 region  

-- Part 1
day12_part1 :: String -> IO [Int]
day12_part1 input = do
    let area = loadArea input
--    printMap area
    let regions = buildRegions area
--    print regions
    let result = foldr (\r acc -> perimeterOfRegion r * sizeOfRegion r + acc) 0 regions
    return [result]

-- Returns a list of all the fence sections around the given region
findFences :: Region -> [Fence]
findFences region = foldr (\p acc -> filter (\(_, p') -> not (Set.member p' region)) (adjacentSection p) ++ acc) [] region
    where
        adjacentSection :: (Int, Int) -> [Fence]
        adjacentSection (i, j) = [(1, (i, j + 1)), (0, (i - 1, j)), (3, (i, j - 1)), (2, (i + 1, j))]

-- Aggregates runs of fence sections
aggregateFences :: [Fence] -> [[Fence]]
aggregateFences = groupFences . sortFences

-- Sorts the fence sections
sortFences :: [Fence] -> [Fence]
sortFences = sortBy fenceSorter
    where
        fenceSorter :: Fence -> Fence -> Ordering
        fenceSorter (d, (i, j)) (d', (i', j'))
            | d /= d'   = compare d d'
            | even d    = compare (i, j) (i', j')
            | otherwise = compare (j, i) (j', i')

-- Groups runs of fence sections
groupFences :: [Fence] -> [[Fence]]
groupFences = foldl (\acc f ->
        if null acc
            then [[f]]
            else
                let g = head acc
                in if fenceGrouper (head g) f
                    then (f : g) : tail acc
                    else [f] : acc
        ) []
    where
        fenceGrouper :: Fence -> Fence -> Bool
        fenceGrouper (d, (i, j)) (d', (i', j'))
            | even d = d == d' && i' == i && j' == j + 1
            | otherwise = d == d' && i' == i + 1 && j' == j

-- Part 2
day12_part2 :: String -> IO [Int]
day12_part2 input = do
    let area = loadArea input
--    printMap area
    let regions = buildRegions area
--    print regions
    let sizes = map sizeOfRegion regions
--    print sizes
    let fences = map (aggregateFences . findFences) regions
--    print fences
    let fenceCounts = map length fences
--    print fenceCounts
    let result = sum $ zipWith (*) fenceCounts sizes
    return [result]
