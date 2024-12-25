module Day15 (
    day15_part1,
    day15_part2
    ) where

import qualified Data.Array as Array

type Array2OfChar = Array.Array (Int, Int) Char

createMap :: [String] -> Array2OfChar
createMap mapLines =
    let bottom = length mapLines - 1
        right = length (head mapLines) - 1
        bounds = ((0, 0), (bottom, right))
        elements = [((i, j), mapLines !! i !! j) | i <- [0 .. bottom], j <- [0 .. right]]
    in Array.array bounds elements

--printMap :: Array2OfChar -> IO ()
--printMap arr = do
--    let ((top, left), (bottom, right)) = Array.bounds arr
--    mapM_ putStrLn [[arr Array.! (i, j) | j <- [left..right]] | i <- [top..bottom]]

findRobot :: Array2OfChar -> (Int, Int)
findRobot area =
    let ((top, left), (bottom, right)) = Array.bounds area
        robot = head [(i, j) | i <- [top..bottom], j <- [left..right], area Array.! (i, j) == '@']
    in robot

move :: (Int, Int) -> (Int, Int) -> Array2OfChar -> Array2OfChar
move from to area =
    let c = area Array.! from
    in area Array.// [(from, '.'), (to, c)]

moveBox :: Array2OfChar -> (Int, Int) -> (Int, Int) -> Maybe Array2OfChar
moveBox area from (di, dj) =
    let to = (fst from + di, snd from + dj)
    in if area Array.! to == '.'
        then Just $ move from to area
        else if area Array.! to == 'O'
            then case moveBox area to (di, dj) of
                Just area' -> Just (move from to area')
                Nothing -> Nothing
            else Nothing

moveRobotIfPossible :: Array2OfChar -> (Int, Int) -> (Int, Int) -> (Array2OfChar, (Int, Int))
moveRobotIfPossible area from (di, dj) =
    let to = (fst from + di, snd from + dj)
    in if area Array.! to == '.'
        then (move from to area, to)
        else if area Array.! to == 'O'
            then case moveBox area to (di, dj) of
                Just area' -> (move from to area', to)
                Nothing -> (area, from)
            else (area, from)

executeSteps :: Array2OfChar -> (Int, Int) -> String -> Array2OfChar
executeSteps area start steps =
    fst $ foldl (\(a, from) s -> moveRobot a from s) (area, start) steps
    where
        moveRobot :: Array2OfChar -> (Int, Int) -> Char -> (Array2OfChar, (Int, Int))
        moveRobot a from step =
            case step of
                '>' -> moveRobotIfPossible a from (0, 1)
                '^' -> moveRobotIfPossible a from (-1, 0)
                '<' -> moveRobotIfPossible a from (0, -1)
                'v' -> moveRobotIfPossible a from (1, 0)
                _ -> error "Invalid step"

coordinateSum :: Array2OfChar  -> Int
coordinateSum area =
    let ((top, left), (bottom, right)) = Array.bounds area
    in sum [i * 100 + j | i <- [top..bottom], j <- [left..right], area Array.! (i, j) == 'O']

-- Part 1
day15_part1 :: String -> IO [Int]
day15_part1 input = do
    let (mapLines, stepLines) = break null (lines input)
    let area = createMap mapLines
--    printMap area
    let steps = concat stepLines
--    putStrLn steps
    let start = findRobot area
--    print start
    let area' = executeSteps area start steps
--    printMap area'
    let result = coordinateSum area'
    return [result]

-- Part 2
day15_part2 :: String -> IO [Int]
day15_part2 input = do
    return []
