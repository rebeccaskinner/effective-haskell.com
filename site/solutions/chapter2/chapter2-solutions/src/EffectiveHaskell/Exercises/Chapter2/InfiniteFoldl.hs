module EffectiveHaskell.Exercises.Chapter2.InfiniteFoldl where
import Prelude hiding (foldl, foldr)

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f acc [] = acc
foldl f acc (x:xs) = foldl f (f acc x) xs

findFirstEvenFoldl :: [Int]
findFirstEvenFoldl =
  foldl firstEven [] [1..]
  where
    firstEven result x
      | even x = [x]
      | otherwise = result

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f acc [] = acc
foldr f acc (x:xs) = f x $ foldr f acc xs

findFirstEvenFoldr :: [Int]
findFirstEvenFoldr =
  foldr firstEven [] [1..]
  where
    firstEven x result
      | even x = [x]
      | otherwise = result
