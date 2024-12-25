module Day17 (
    day17_part1,
    day17_part2
    ) where

import qualified Data.Array as Array
import Data.Bits (xor)
import Data.Char (isDigit)
import Data.List (intercalate)
import Data.List.Split (splitOn)
--import Debug.Trace ( trace, traceShow )
import Text.Regex.TDFA ( (=~) )

type Program = [Int]
type Registers = Array.Array Int Int

type Instruction = Int -> State -> State
type InstructionSet = Array.Array Int Instruction

data State = State {
    regs :: Registers,
    ip :: Int,
    output :: [Int]
} deriving (Show)

regA :: Int
regA = 0
regB :: Int
regB = 1
regC :: Int
regC = 2

combo :: Registers -> Int -> Int
combo registers operand = if operand < 4 then operand else registers Array.! (operand - 4)

updateState :: State -> Int -> Int -> Int -> State
updateState state reg value i = state { regs = regs state Array.// [(reg, value)], ip = i }

-- The adv instruction (opcode 0) performs division. The numerator is the value in the A register. The denominator is
-- found by raising 2 to the power of the instruction's combo operand. (So, an operand of 2 would divide A by 4 (2^2);
-- an operand of 5 would divide A by 2^B.) The result of the division operation is truncated to an integer and then
-- written to the A register.
adv :: Int -> State -> State
adv operand state =
    let o1 = regs state Array.! regA
        o2 = combo (regs state) operand
    in updateState state regA (o1 `div` (2 ^ o2)) (ip state + 2)

-- The bxl instruction (opcode 1) calculates the bitwise XOR of register B and the instruction's literal operand, then
-- stores the result in register B.
bxl :: Int -> State -> State
bxl operand state =
    let o1 = regs state Array.! regB
        o2 = operand
    in updateState state regB (o1 `xor` o2) (ip state + 2)

-- The bst instruction (opcode 2) calculates the value of its combo operand modulo 8 (thereby keeping only its lowest 3
-- bits), then writes that value to the B register.
bst :: Int -> State -> State
bst operand state =
    let o1 = combo (regs state) operand
    in updateState state regB (o1 `mod` 8) (ip state + 2)

-- The jnz instruction (opcode 3) does nothing if the A register is 0. However, if the A register is not zero, it jumps
-- by setting the instruction pointer to the value of its literal operand; if this instruction jumps, the instruction
-- pointer is not increased by 2 after this instruction.
jnz :: Int -> State -> State
jnz operand state =
    let o1 = regs state Array.! regA
    in if o1 /= 0
         then state { ip = operand}
         else state { ip = ip state + 2}

-- The bxc instruction (opcode 4) calculates the bitwise XOR of register B and register C, then stores the result in
-- register B. (For legacy reasons, this instruction reads an operand but ignores it.)
bxc :: Int -> State -> State
bxc _ state =
    let o1 = regs state Array.! regB
        o2 = regs state Array.! regC
    in updateState state regB (o1 `xor`o2) (ip state + 2)

-- The out instruction (opcode 5) calculates the value of its combo operand modulo 8, then outputs that value. (If a
-- program outputs multiple values, they are separated by commas.)
out :: Int -> State -> State
out operand state =
    let o1 = combo (regs state) operand
    in state { output = (o1 `mod` 8):output state, ip = ip state + 2}

-- The bdv instruction (opcode 6) works exactly like the adv instruction except that the result is stored in the B
-- register. (The numerator is still read from the A register.)
bdv :: Int -> State -> State
bdv operand state =
    let o1 = regs state Array.! regA
        o2 = combo (regs state) operand
    in updateState state regB (o1 `div` (2 ^ o2)) (ip state + 2)

-- The cdv instruction (opcode 7) works exactly like the adv instruction except that the result is stored in the C
-- register. (The numerator is still read from the A register.)
cdv :: Int -> State -> State
cdv operand state =
    let o1 = regs state Array.! regA
        o2 = combo (regs state) operand
    in updateState state regC (o1 `div` (2 ^ o2)) (ip state + 2)

instructions :: InstructionSet
instructions = Array.listArray (0, 7) [ adv, bxl, bst, jnz, bxc, out, bdv, cdv ]

execute :: State -> Program -> [Int]
execute state0 program =
    reverse (output (exec state0))
    where
        exec :: State -> State
        exec state
                | ip state >= length program = state
                | otherwise = 
                    let i = ip state
                        opcode = program !! i
                        operand = program !! (i + 1)
                        instruction = instructions Array.! opcode
                        state' = instruction operand state
                    in exec state'
-- Part 1
day17_part1 :: String -> IO [Int]
day17_part1 input = do
    let (registerLines, programLines) = break null $ lines input
    let registers = Array.listArray (0, 2) $ map (\s -> read (s =~ "[0-9]+") :: Int) registerLines :: Registers
--    print registers
    let program = map (\s -> read s :: Int) $ splitOn "," $ dropWhile (not . isDigit) (head $ tail programLines) :: Program
--    print program
    let result = execute (State registers 0 []) program
    putStrLn ""
    print $ intercalate "," $ map show result
    return result

{-
disassemble :: Program -> [String]
disassemble program =
    next program 0
        where
            next :: Program -> Int -> [String]
            next [] _ = []
            next [_] _ = []
            next (opcode:operand:rest) i =
                (show i ++ " " ++ instructionNames !! opcode ++ " " ++ decodeOperand) : next rest (i + 1)
                where
                    instructionNames = ["adv", "bxl", "bst", "jnz", "bxc", "out", "bdv", "cdv"]
                    decodeOperand :: String
                    decodeOperand
                        | opcode == 1 || opcode == 3 = show operand
                        | opcode == 4 = ""
                        | otherwise = if operand < 4 then show operand else "r" ++ (["A","B","C"] !! (operand - 4))
-}

search :: Program -> Int -> [Int] -> Int
search program i0 v =
    i0 + head (filter (\i -> execute (State (Array.listArray (0, 2) [i0 + i, 0, 0]) 0 []) program == v) [0..])

solve :: Program -> Int
solve program =
    next 0 [] (reverse program)
        where
            next :: Int -> [Int] -> [Int]-> Int
            next i0 _ [] = i0
            next i0 right left = -- Note: left is reversed for easier access
                let (p:left') = left
                    right' = p : right
                    i1 = search program (i0 * 8) right'
                in next i1 right' left'
    
-- Part 2
day17_part2 :: String -> IO [Int]
day17_part2 input = do
    let (_, programLines) = break null $ lines input
    let program = map (\s -> read s :: Int) $ splitOn "," $ dropWhile (not . isDigit) (head $ tail programLines) :: Program
--    print program
--    let asm = disassemble program
--    mapM_ putStrLn asm
    let result = solve program
    return [result]
