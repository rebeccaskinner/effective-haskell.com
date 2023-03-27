module EffectiveHaskell.Exercises.Chapter2.ReverseWithFold where

reverseLeft :: [a] -> [a]
reverseLeft = foldl (flip (:)) []

reverseRight :: [a] -> [a]
reverseRight = foldr insertElem []
  where
    insertElem a reversed = reversed <> [a]
