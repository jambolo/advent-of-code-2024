module Day05 (
    day05_part1,
    day05_part2
    ) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Text.Read (readMaybe)
import Data.List.Split (splitOn)

type AfterMap = Map.Map Int [Int]
type FoundSet = Set.Set Int

splitAround :: Eq a => a -> [a] -> Maybe ([a], a, [a])
splitAround _ [] = Nothing
splitAround x xs = case break (== x) xs of
    (before, s : after) -> Just (before, s, after)
    _ -> Nothing

splitIntoSections :: String -> ([String], [String])
splitIntoSections input = case splitAround "" (lines input) of
    Just (before, _, after) -> (before, after)
    _ -> error $ "No empty line in file"

parseOrderingEntry :: String -> (Int, Int)
parseOrderingEntry s = case mapM readMaybe (splitOn "|" s) of
    Just [key, value] -> (key, value)
    _ -> error $ "Unable to parse ordering entry"

buildAfterMap :: [(Int, Int)] -> AfterMap
buildAfterMap  = foldr (\(k, v) -> Map.insertWith (++) k [v]) Map.empty

parseUpdate :: String -> [Int]
parseUpdate s = case mapM readMaybe (splitOn "," s) of
    Just numbers -> numbers
    _ -> error "Unable to parse numbers"

middleValue :: [Int] -> Int
middleValue a = a !! (length (a) `div` 2)

anyInFound :: FoundSet -> [Int] -> Bool
anyInFound set = any (`Set.member` set)

orderIsValidRecursive :: AfterMap -> FoundSet -> [Int] -> Bool
orderIsValidRecursive _ _ [] = True
orderIsValidRecursive aftermap found (x:xs) = 
    case Map.lookup x aftermap of
        -- If the page is in the ordering rules
        Just after -> 
            -- then if any of the after values have already been encountered
            if anyInFound found after
            -- then return not valid
            then False
            -- otherwise add this page to the found set and continue with the rest of the pages
            else orderIsValidRecursive aftermap (Set.insert x found) xs
        -- If the page is not in the ordering rules then just continue
        Nothing -> orderIsValidRecursive aftermap (Set.insert x found) xs

orderIsValid :: AfterMap -> [Int] -> Bool
orderIsValid aftermap = orderIsValidRecursive aftermap Set.empty

day05_part1 :: String -> IO Int
day05_part1 input = do
    let (orderingInput, updatesInput) = splitIntoSections input
    let afterMap = buildAfterMap $ map parseOrderingEntry orderingInput
    let updates = map parseUpdate updatesInput
    let result = sum $ map middleValue $ filter (\x -> orderIsValid afterMap x) updates
    return result

day05_part2 :: String -> IO Int
day05_part2 input = do
    return 0
