module EffectiveHaskell.Exercises.Chapter2.ConcatMap where
import Prelude hiding (foldl, foldr)

concatMapFoldl :: (a -> [b]) -> [a] -> [b]
concatMapFoldl f = foldl (\acc x -> acc <> f x) []
-- concatMapFoldl f = foldl (\acc x -> f x <> acc) []

concatMapFoldr :: (a -> [b]) -> [a] -> [b]
concatMapFoldr f = foldr (\x acc -> f x <> acc) []

concatFoldr :: [[a]] -> [a]
concatFoldr = foldr (<>) []

concatFoldl :: [[a]] -> [a]
concatFoldl = foldl (<>) []

mapFoldr :: (a -> b) -> [a] -> [b]
mapFoldr f = foldr (\x acc -> f x : acc) []

mapFoldl :: (a -> b) -> [a] -> [b]
mapFoldl f = foldl (\acc x -> f x : acc) []

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f acc [] = acc
foldl f acc (x:xs) = foldl f (f acc x) xs

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f acc [] = acc
foldr f acc (x:xs) = f x (foldr f acc xs)
