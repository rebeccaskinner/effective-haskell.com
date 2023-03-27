module EffectiveHaskell.Exercises.Chapter1.Factorial where

factorial :: Integer -> Integer
factorial n
  | n <= 1 = 1
  | otherwise = n * factorial (n - 1)
