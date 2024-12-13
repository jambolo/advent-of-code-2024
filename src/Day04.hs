module Day04 (
    day04_part1,
    day04_part2
    ) where

import qualified Data.Array as Array

type Array2OfChar = Array.Array (Int, Int) Char
type Rect = ((Int, Int), (Int, Int))

createPuzzle :: [[Char]] -> Array2OfChar
createPuzzle a =
    let nRows = length a
        nCols = length (head a)
        bounds = ((0, 0), (nRows - 1, nCols - 1))
        elements = [((i, j), a !! i !! j) | i <- [0 .. nRows - 1], j <- [0 .. nCols - 1]]
    in Array.array bounds elements

checkXmas :: Array2OfChar -> (Int, Int) -> (Int, Int) -> Bool
checkXmas puzzle (i, j) (di, dj) =  puzzle Array.! (i + 0 * di, j + 0 * dj) == 'X' &&
                                    puzzle Array.! (i + 1 * di, j + 1 * dj) == 'M' &&
                                    puzzle Array.! (i + 2 * di, j + 2 * dj) == 'A' &&
                                    puzzle Array.! (i + 3 * di, j + 3 * dj) == 'S'


findMatches :: Array2OfChar -> (Int, Int) -> Rect -> [Bool]
findMatches puzzle d ((top, left), (bottom, right)) = [checkXmas puzzle (i, j) d | i <- [top .. bottom], j <- [left .. right]]

countMatches :: Array2OfChar -> (Int, Int) -> Rect -> Int
countMatches puzzle d bounds = length . filter id $ findMatches puzzle d bounds

day04_part1 :: String -> IO Int
day04_part1 input = do
    let puzzle = createPuzzle $ lines input
    let ((top, left), (bottom, right)) = Array.bounds puzzle
    let total = countMatches puzzle ( 0,  1) ((top    , left    ), (bottom    , right - 3)) +
                countMatches puzzle (-1,  1) ((top + 3, left    ), (bottom    , right - 3)) +
                countMatches puzzle (-1,  0) ((top + 3, left    ), (bottom    , right    )) +
                countMatches puzzle (-1, -1) ((top + 3, left + 3), (bottom    , right    )) +
                countMatches puzzle ( 0, -1) ((top    , left + 3), (bottom    , right    )) +
                countMatches puzzle ( 1, -1) ((top    , left + 3), (bottom - 3, right    )) +
                countMatches puzzle ( 1,  0) ((top    , left    ), (bottom - 3, right    )) +
                countMatches puzzle ( 1,  1) ((top    , left    ), (bottom - 3, right - 3))
    return total

checkX :: Array2OfChar -> (Int, Int) -> Bool
checkX puzzle (i, j) = puzzle Array.! (i + 0, j + 0) == 'A' &&
                       (puzzle Array.! (i - 1, j - 1) == 'M' && puzzle Array.! (i + 1, j + 1) == 'S' ||
                        puzzle Array.! (i - 1, j - 1) == 'S' && puzzle Array.! (i + 1, j + 1) == 'M') &&
                       (puzzle Array.! (i - 1, j + 1) == 'M' && puzzle Array.! (i + 1, j - 1) == 'S' ||
                        puzzle Array.! (i - 1, j + 1) == 'S' && puzzle Array.! (i + 1, j - 1) == 'M')

findXs :: Array2OfChar -> Rect -> [Bool]
findXs puzzle ((top, left), (bottom, right)) = [checkX puzzle (i, j) | i <- [top .. bottom], j <- [left .. right]]

day04_part2 :: String -> IO Int
day04_part2 input = do
    let puzzle = createPuzzle $ lines input
    let ((top, left), (bottom, right)) = Array.bounds puzzle
    let total = length . filter id $ findXs puzzle ((top + 1, left + 1), (bottom - 1, right - 1))
    return total