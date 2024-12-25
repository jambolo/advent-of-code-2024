module Day20 (
    day20_part1,
    day20_part2
    ) where

--import Debug.Trace ( trace )
import qualified Data.Array as Array

type Array2OfChar = Array.Array (Int, Int) Char
type Array2OfInt = Array.Array (Int, Int) Int

createMap :: String -> Array2OfChar
createMap input =
    let a = lines input
        bottom = length a - 1
        right = length (head a) - 1
        bounds = ((0, 0), (bottom, right))
        elements = [((i, j), a !! i !! j) | i <- [0 .. bottom], j <- [0 .. right]]
    in Array.array bounds elements

{-
printArray2OfChar :: Array2OfChar -> IO ()
printArray2OfChar area = do
    let ((top, left), (bottom, right)) = Array.bounds area
    mapM_ (\i -> do
        mapM_ (\j -> putChar (area Array.! (i, j))) [left..right]
        putChar '\n'
        ) [top..bottom]

printArray2OfInt :: Array2OfInt -> IO ()
printArray2OfInt distances = do
    let ((top, left), (bottom, right)) = Array.bounds distances
    mapM_ (\i -> do
        mapM_ (\j -> 
            let d = distances Array.! (i, j)
            in putStr ((if d < 0 then "##" else if d >= 0 && d < 10 then " " ++ show d else show d) ++ " ")) [left..right]
        putChar '\n'
        ) [top..bottom]

-}
fillArray :: ((Int, Int), (Int, Int)) -> Int -> Array2OfInt
fillArray bounds value =
    let ((top, left), (bottom, right)) = bounds
    in Array.array bounds [((i, j), value) | i <- [top..bottom], j <- [left..right]]

findChar :: Array2OfChar -> Char -> (Int, Int)
findChar area c =
    head [p | p <- Array.indices area, area Array.! p == c]

distance :: (Int, Int) -> (Int, Int) -> Int
distance (i1, j1) (i2, j2) = abs (i1 - i2) + abs (j1 - j2)

findDistances :: Array2OfChar -> (Int, Int) -> Array2OfInt
findDistances area start =
    let ((top, left), (bottom, right)) = Array.bounds area
        distances = fillArray ((top, left), (bottom, right)) (-1)
        queue = [(start, 0)]
    in f distances queue
    where
        f :: Array2OfInt -> [((Int, Int), Int)] -> Array2OfInt
        f distances' [] = distances'
        f distances' ((p, d'):queue') =
            let d = distances' Array.! p
                (i, j) = p
                adjacent = [(i, j + 1), (i - 1, j), (i, j - 1), (i + 1, j)]
                neighbors = [p' | p' <- adjacent, Array.inRange (Array.bounds area) p', area Array.! p' /= '#']
            in if d < 0 || d' < d
                then f (distances' Array.// [(p, d')]) (queue' ++ [(n, d' + 1) | n <- neighbors])
                else f distances' queue'

generateNeighbors :: (Int, Int) -> Int -> [(Int, Int)]
generateNeighbors (i, j) n = [(i + x, j + y) | x <- [-n..n], y <- [-n..n], abs x + abs y <= n]

findShortcuts :: Array2OfInt -> Int -> Int -> [(((Int, Int), (Int, Int)), Int)]
findShortcuts distances minShortcut time =
    let bounds = Array.bounds distances
    in foldr (\p acc ->
        let d = distances Array.! p
            adjacent = generateNeighbors p time
            neighbors = [p' | p' <- adjacent, Array.inRange bounds p', distances Array.! p' >= 0]
        in foldr (\p' acc' ->
            let d' = distances Array.! p'
            in if d' - d - distance p' p >= minShortcut
                then ((p, p'), d' - d - distance p' p) : acc'
                else acc'
            ) acc neighbors
        ) [] [p | p <- Array.indices distances, distances Array.! p >= 0]

-- Part 1
day20_part1 :: String -> IO [Int]
day20_part1 input = do
    let area = createMap input
--    printArray2OfChar area
--    let start = findChar area 'S'
    let end = findChar area 'E'
--    print start
--    print end
    let distances = findDistances area end
--    print distances
--    printArray2OfInt distances
    let shortcuts = findShortcuts distances 100 2
--    print shortcuts
    let result = length shortcuts
    return [result]

-- Part 2
day20_part2 :: String -> IO [Int]
day20_part2 input = do
    let area = createMap input
--    printArray2OfChar area
--    let start = findChar area 'S'
    let end = findChar area 'E'
--    print start
--    print end
    let distances = findDistances area end
--    print distances
--    printArray2OfInt distances
    let shortcuts = findShortcuts distances 100 20
--    print shortcuts
    let result = length shortcuts
    return [result]
