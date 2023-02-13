module EffectiveHaskell.Exercises.Chapter2.ReverseWithFold where

-- reverseLeft :: [a] -> [a]
-- reverseLeft = foldl (flip (:)) []

reverseLeft :: [a] -> [a]
reverseLeft = foldl prepend []
  where
    prepend reversed a = a : reversed

reverseRight :: [a] -> [a]
reverseRight = foldr append []
  where
    append a reversed = reversed <> [a]
