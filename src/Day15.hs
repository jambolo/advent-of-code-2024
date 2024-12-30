module Day15 (
    day15_part1,
    day15_part2
    ) where

import qualified Data.Array as Array
import Data.Maybe (catMaybes, isNothing)
import Data.List (group, sort)

type Array2OfChar = Array.Array (Int, Int) Char

createMap :: [String] -> Array2OfChar
createMap mapLines =
    let bottom = length mapLines - 1
        right = length (head mapLines) - 1
        bounds = ((0, 0), (bottom, right))
        elements = [((i, j), mapLines !! i !! j) | i <- [0 .. bottom], j <- [0 .. right]]
    in Array.array bounds elements

{-
printMap :: Array2OfChar -> IO ()
printMap arr = do
    let ((top, left), (bottom, right)) = Array.bounds arr
    mapM_ putStrLn [[arr Array.! (i, j) | j <- [left..right]] | i <- [top..bottom]]
-}

findRobot :: Array2OfChar -> (Int, Int)
findRobot area =
    let ((top, left), (bottom, right)) = Array.bounds area
        robot = head [(i, j) | i <- [top..bottom], j <- [left..right], area Array.! (i, j) == '@']
    in robot

move :: Array2OfChar -> (Int, Int) -> (Int, Int) -> Array2OfChar
move area from to =
    let c = area Array.! from
    in area Array.// [(from, '.'), (to, c)]


moveBoxes :: Array2OfChar -> (Int, Int) -> (Int, Int) -> Maybe Array2OfChar
moveBoxes area from (di, dj) =
    let to = (fst from + di, snd from + dj)
    in if area Array.! to == '.'
        then Just $ move area from to
        else if area Array.! to == 'O'
            then case moveBoxes area to (di, dj) of
                Just area' -> Just (move area' from to)
                Nothing -> Nothing
            else Nothing

moveRobotIfPossible :: Array2OfChar -> (Int, Int) -> (Int, Int) -> (Array2OfChar, (Int, Int))
moveRobotIfPossible area from (di, dj) =
    let to = (fst from + di, snd from + dj)
    in if area Array.! to == '.'
        then (move area from to, to)
        else if area Array.! to == 'O'
            then case moveBoxes area to (di, dj) of
                Just area' -> (move area' from to, to)
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

symbol :: [[Char]] -> Int -> Int -> Char
symbol mapLines i j =
    let c = mapLines !! i !! (j `div` 2)
        k = j `mod` 2
    in case c of
        '#' -> "##" !! k
        '.' -> ".." !! k
        '@' -> "@." !! k
        'O' -> "[]" !! k
        _ -> '?'


createMap2 :: [String] -> Array2OfChar
createMap2 mapLines =
    let bottom = length mapLines - 1
        right = length (head mapLines) * 2 - 1
        bounds = ((0, 0), (bottom, right))
        elements = [((i, j), symbol mapLines i j) | i <- [0 .. bottom], j <- [0 .. right]]
    in Array.array bounds elements

boxCoord :: (Int, Int) -> Char ->(Int, Int)
boxCoord (i, j) c = case c of
    '[' -> (i, j)
    ']' -> (i, j - 1)
    _ -> error "Invalid box character"

moveBox :: Array2OfChar -> (Int, Int) -> (Int, Int) -> Array2OfChar
moveBox area f0 t0 =
    let f1 = (fst f0, snd f0 + 1)
        t1 = (fst t0, snd t0 + 1)
    in (area Array.// [(f0, '.'), (f1, '.')]) Array.// [(t0, '['), (t1, ']')]

dedup :: [(Int, Int)] -> [(Int, Int)]
dedup = map head . group . sort

moveBoxes2 :: Array2OfChar -> [(Int, Int)] -> (Int, Int) -> Maybe Array2OfChar
moveBoxes2 area from (di, dj) =
    let to = map (\(i, j) -> (i + di, j + dj)) from
        -- next boxes to be moved first
        nextBoxes = dedup $ foldr (\t0 acc ->
            let t1 = (fst t0 , snd t0 + 1)
                c0 = area Array.! t0
                c1 = area Array.! t1
                b0 = if (dj /= 1)    && (c0 == '[' || c0 == ']') then Just (boxCoord t0 c0) else Nothing
                b1 = if (dj /= (-1)) && (c1 == '[' || c1 == ']') then Just (boxCoord t1 c1) else Nothing
            in catMaybes [b0, b1] ++ acc
            ) [] to
        -- area after moving the next boxes (if any)
        area' = if null nextBoxes then Just area else moveBoxes2 area nextBoxes (di, dj)
    in if isNothing area'
        then area' -- Nothing
        else foldr (\(f0, t0) acc ->
            let t1 = (fst t0 , snd t0 + 1)
            in case acc of
                Just a -> if a Array.! t0 /= '#' && a Array.! t1 /= '#'
                    then Just $ moveBox a f0 t0
                    else Nothing
                Nothing -> Nothing
            ) area' $ zip from to

moveRobotIfPossible2 :: Array2OfChar -> (Int, Int) -> (Int, Int) -> (Array2OfChar, (Int, Int))
moveRobotIfPossible2 area from (di, dj) =
    let to = (fst from + di, snd from + dj)
        c = area Array.! to
    in
        if c == '.' then (move area from to, to)
        else if c == '[' || c == ']' then
            case moveBoxes2 area [boxCoord to c] (di, dj) of
                Just area' -> (move area' from to, to)
                Nothing -> (area, from) -- can't move because boxes can't move
        else (area, from)

executeSteps2 :: Array2OfChar -> (Int, Int) -> String -> Array2OfChar
executeSteps2 area start steps =
    fst $ foldl moveRobot (area, start) steps
    where
        moveRobot :: (Array2OfChar, (Int, Int)) -> Char -> (Array2OfChar, (Int, Int))
        moveRobot (a, from) step =
            moveRobotIfPossible2 a from d
                where
                    d = case step of
                        '>' -> (0, 1)
                        '^' -> (-1, 0)
                        '<' -> (0, -1)
                        'v' -> (1, 0)
                        _ -> error "Invalid step"

coordinateSum2 :: Array2OfChar  -> Int
coordinateSum2 area =
    let ((top, left), (bottom, right)) = Array.bounds area
    in sum [i * 100 + j | i <- [top..bottom], j <- [left..right], area Array.! (i, j) == '[']


-- Part 2
day15_part2 :: String -> IO [Int]
day15_part2 input = do
    let (mapLines, stepLines) = break null (lines input)
    let area = createMap2 mapLines
--    printMap area
    let steps = concat stepLines
--    putStrLn steps
    let start = findRobot area
--    print start
    let area' = executeSteps2 area start steps
--    printMap area'
    let result = coordinateSum2 area'
    return [result]
