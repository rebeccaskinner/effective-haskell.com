module EffectiveHaskell.Exercises.Chapter2.InfiniteFolds where

-- foldr func carryValue lst =
--   if null lst
--   then carryValue
--   else func (head lst) $ EffectiveHaskell.Exercises.Chapter2.InfiniteFolds.foldr func carryValue (tail lst)

-- foldl :: (b -> a -> b) -> b -> [a] -> b
-- foldl func carryValue lst =
--   if null lst
--   then carryValue
--   else foldl func (func carryValue (head lst)) (tail lst)

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

searchSumFoldl :: Int -> [Int] -> Int
searchSumFoldl limit = foldl testLimit 0
  where
    testLimit acc n =
      if acc >= limit
      then limit
      else acc + n

searchSumFoldr :: Int -> [Int] -> Int
searchSumFoldr limit = foldr testLimit 0
  where
    testLimit n acc =
      if acc >= limit
      then limit
      else n + acc

searchElemFoldl :: Int -> [Int] -> Int
searchElemFoldl limit = foldl testLimit 0
  where
    testLimit acc n =
      if n >= limit
      then n
      else acc + n

searchElemFoldr :: Int -> [Int] -> Int
searchElemFoldr limit = foldr testLimit 0
  where
    testLimit n acc =
      if n >= limit
      then n
      else n + acc

findFirst :: (Int -> Bool) -> [Int] -> [Int]
findFirst predicate =
  foldr findHelper []
  where
    findHelper listElement maybeFound
      | predicate listElement = [listElement]
      | otherwise = maybeFound

findFirstCompose :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
findFirstCompose predicate g =
  foldr (findHelper . g) []
  where
    findHelper listElement maybeFound
      | predicate listElement = [listElement]
      | otherwise = maybeFound

findFirstMap :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
findFirstMap predicate g =
  foldr findHelper [] . map g
  where
    findHelper listElement maybeFound
      | predicate listElement = [listElement]
      | otherwise = maybeFound
