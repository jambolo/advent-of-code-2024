module Answer (
    Answer(..)
    ) where

data Answer = Ints [Int] | Str String deriving (Eq)

instance Show Answer where
    show (Ints xs) = show xs
    show (Str s)   = s
