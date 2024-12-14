module Day14 (
    day14_part1,
    day14_part2
    ) where

import Text.Regex.TDFA

data Robot = Robot {
    p :: (Int, Int),
    v :: (Int, Int)
} deriving (Show)

width :: Int
height :: Int

-- -- Example width and height
-- width = 11
-- height = 7

-- Real width and height
width = 101
height = 103

-- Load robots from input
loadRobots :: [String] -> [Robot]
loadRobots = map loadRobot
    where
        loadRobot :: String -> Robot
        loadRobot string =
            let robotRE = "p=(-?[0-9]+),(-?[0-9]+) v=(-?[0-9]+),(-?[0-9]+)"
            in case string =~ robotRE :: (String, String, String, [String]) of
                (_, _, _, [px, py, vx, vy]) -> Robot (read px, read py) (read vx, read vy)
                _ -> error $ "Unable to parse: " ++ string

-- Move the robots for n seconds
moveRobots :: Int -> [Robot] -> [Robot]
moveRobots n = map moveRobot
    where
        moveRobot :: Robot -> Robot
        moveRobot (Robot (px, py) (vx, vy)) = Robot ((px + n * vx) `mod` width, (py + n * vy) `mod` height) (vx, vy)

{-
-- Prints a map showing the locations of the robots
printMap :: [Robot] -> IO ()
printMap robots = do
    let robotMap = foldl (\m (Robot (px, py) _) -> set m px py) (replicate height (replicate width '.')) robots
    mapM_ putStrLn robotMap
    where
        set :: [[Char]] -> Int -> Int -> [[Char]]
        set m x y = take y m ++ [setRow (m !! y) x] ++ drop (y + 1) m
        setRow :: [Char] -> Int -> [Char]
        setRow r x = take x r ++ ['X'] ++ drop (x + 1) r
-}

-- Counts the number of robots in each quadrant
census :: [Robot] -> (Int, Int, Int, Int)
census = foldl count (0, 0, 0, 0)
    where
        count :: (Int, Int, Int, Int) -> Robot -> (Int, Int, Int, Int)
        count (q1, q2, q3, q4) (Robot (px, py) _) = (q1 + q1', q2 + q2', q3 + q3', q4 + q4')
            where
                q1' = if px < width `div` 2 && py < height `div` 2 then 1 else 0
                q2' = if px > width `div` 2 && py < height `div` 2 then 1 else 0
                q3' = if px < width `div` 2 && py > height `div` 2 then 1 else 0
                q4' = if px > width `div` 2 && py > height `div` 2 then 1 else 0

safetyFactor :: (Int, Int, Int, Int) -> Int
safetyFactor (q1, q2, q3, q4) = q1 * q2 * q3 * q4


-- Part 1
day14_part1 :: String -> IO Int
day14_part1 input = do
    let robots = loadRobots (lines input)
--    print robots
--    printMap robots
    let movedRobots = moveRobots 100 robots
--    print movedRobots
--    printMap movedRobots
    let c = census movedRobots
--    print c
    return $ safetyFactor c

-- Part 2
day14_part2 :: String -> IO Int
day14_part2 input = do
    return 0
