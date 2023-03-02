module EffectiveHaskell.Exercises.Chapter2.InfiniteFolds where

-- foldr func carryValue lst =
--   if null lst
--   then carryValue
--   else func (head lst) $ EffectiveHaskell.Exercises.Chapter2.InfiniteFolds.foldr func carryValue (tail lst)

double :: Int -> Int
double n = n * 2

add :: Int -> Int -> Int
add a b = a + b

composeMapFoldr :: (a -> b) -> (b -> Int -> Int) -> [a] -> Int
composeMapFoldr f g = foldr g 0 . map f

composeFandGFoldr :: (a -> b) -> (b -> Int -> Int) -> [a] -> Int
composeFandGFoldr f g = foldr (g . f) 0

composeMapFoldl :: (a -> b) -> (Int -> b -> Int) -> [a] -> Int
composeMapFoldl f g = foldl g 0 . map f

composeFandGFoldl :: (Int -> Int) -> (Int -> a -> Int) -> [a] -> Int
composeFandGFoldl f g = foldl (g . f) 0

composeFandGFoldl' :: (a -> b) -> (Int -> b -> Int) -> [a] -> Int
composeFandGFoldl' f g = foldl (\acc -> g acc . f) 0
