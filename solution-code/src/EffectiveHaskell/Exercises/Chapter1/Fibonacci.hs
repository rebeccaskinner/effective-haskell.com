module EffectiveHaskell.Exercises.Chapter1.Fibonacci where

fibonacci :: Int -> Int
fibonacci n
  | n <= 0 = 0
  | n == 1 = 1
  | otherwise = fibonacci (n - 1) + fibonacci (n - 2)
