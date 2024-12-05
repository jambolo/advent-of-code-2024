module Main (main) where

import System.Environment (getArgs)
import Day01

type StringsToNumber = [String] -> Int

dayTable :: [(String, StringsToNumber)]
dayTable = [("day01", day01_part1)]

main :: IO ()
main = do
    args <- getArgs
    case args of
        [day, inputName] -> do
            putStrLn $ "Day " ++ day ++ ", input: " ++ inputName
            input <- readFile inputName
            let linesOfInput = lines input
            case lookup day dayTable of
                Nothing -> error $ "Unknown day: " ++ day
                Just solution -> do
                    let result = solution linesOfInput
                    putStrLn $ day ++ " solution: " ++ show result
          
        _ -> error "Usage: AdventOfCode2024 <day> <input file name>"
