module Day05 (
    day05_part1,
    day05_part2
    ) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Text.Read (readMaybe)
import Data.List.Split (splitOn)
import Data.List (sortBy)


type AfterMap = Map.Map Int [Int]
type FoundSet = Set.Set Int

splitIntoSections :: String -> ([String], [String])
splitIntoSections input =
    let (before, after) = break null (lines input)
    in (before, tail after)

parseOrderingEntry :: String -> (Int, Int)
parseOrderingEntry s = case mapM readMaybe (splitOn "|" s) of
    Just [key, value] -> (key, value)
    _ -> error "Unable to parse ordering entry"

buildAfterMap :: [(Int, Int)] -> AfterMap
buildAfterMap  = foldr (\(k, v) -> Map.insertWith (++) k [v]) Map.empty

parseUpdate :: String -> [Int]
parseUpdate s = case mapM readMaybe (splitOn "," s) of
    Just numbers -> numbers
    _ -> error "Unable to parse numbers"

middleValue :: [Int] -> Int
middleValue a = a !! (length a `div` 2)

anyInFound :: FoundSet -> [Int] -> Bool
anyInFound set = any (`Set.member` set)

orderIsValid :: AfterMap -> [Int] -> Bool
orderIsValid afterMap =
    next Set.empty
    where
        next :: FoundSet -> [Int] -> Bool
        next _ [] = True
        next found (x:xs) = 
            case Map.lookup x afterMap of
                -- If the page is in the ordering rules
                Just after -> 
                    -- then if any of the after values have already been encountered
                    not (anyInFound found after) && next (Set.insert x found) xs
                -- If the page is not in the ordering rules then just continue
                Nothing -> next (Set.insert x found) xs

day05_part1 :: String -> IO Int
day05_part1 input = do
    let (orderingInput, updatesInput) = splitIntoSections input
        afterMap = buildAfterMap $ map parseOrderingEntry orderingInput
        updates = map parseUpdate updatesInput
        result = sum $ map middleValue $ filter (orderIsValid afterMap) updates
    return result

pageCompare :: AfterMap -> Int -> Int -> Ordering
pageCompare afterMap x y
    | x == y = EQ
    | x `elem` Map.findWithDefault [] y afterMap = GT
    | y `elem` Map.findWithDefault [] x afterMap = LT
    | otherwise = EQ

sortUpdate :: AfterMap -> [Int] -> [Int]
sortUpdate afterMap = sortBy (pageCompare afterMap)

day05_part2 :: String -> IO Int
day05_part2 input = do
    let (orderingInput, updatesInput) = splitIntoSections input
        afterMap = buildAfterMap $ map parseOrderingEntry orderingInput
        updates = map parseUpdate updatesInput
        sortedInvalidUpdates = map (sortUpdate afterMap) $ filter (not . orderIsValid afterMap) updates
        result = sum $ map middleValue sortedInvalidUpdates
    return result
