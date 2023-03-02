module EffectiveHaskell.Exercises.Chapter2.ZippingLists where

exampleZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
-- exampleZipWith f [] bs = []
-- exampleZipWith f as [] = []
exampleZipWith f (a:as) (b:bs) = f a b : exampleZipWith f as bs
exampleZipWith _f _as _bs = []

exampleZipWithCase f a b =
  case (a,b) of
    (a':as, b':bs) -> f a' b' : exampleZipWith f as bs
    _ -> []

exampleGuard f as bs
  | null as || null bs = []
  | otherwise = f (head as) (head bs) : exampleGuard f (tail as) (tail bs)


examplePatternGuard f as bs
  | (a:as') <- as, (b:bs') <- bs = f a b : examplePatternGuard f as' bs'
  | otherwise = []

exampleZipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
exampleZipWith' f as bs
  | (a:as') <- as, (b:bs') <- bs = f a b : exampleZipWith' f as' bs'
  | otherwise = []

exampleZipWithComprehensionBad :: (a -> b -> c) -> [a] -> [b] -> [c]
exampleZipWithComprehensionBad f as bs = [f a b | a <- as, b <- bs]

exampleZipWithComprehension :: (a -> b -> c) -> [a] -> [b] -> [c]
exampleZipWithComprehension f as bs = [f a b | (a,b) <- zip as bs]

exampleZipWithComprehensionIndex :: (a -> b -> c) -> [a] -> [b] -> [c]
exampleZipWithComprehensionIndex f as bs =
  [ f (as !! idx) (bs !! idx)
  | idx <- [0 .. len - 1]
  ]
  where
    len = min (length as) (length bs)
-- exampleZipWithComprehensionIndex f as bs =
--   [f (as !! (idx - 1)) (bs !! (idx - 1)) | idx <- [1..len]]
--   where
--     len = min (length as) (length bs)

zipWithFoldl :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWithFoldl f as bs = reverse $ foldl applyFunction [] zipped
  where
    zipped = zip as bs
    applyFunction accumulator (a,b) = f a b : accumulator

zipWithFoldr :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWithFoldr f as bs = foldr applyFunction [] zipped
  where
    zipped = zip as bs
    applyFunction (a,b) accumulator = f a b : accumulator

zipWithFoldl' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWithFoldl' f as bs =
  reverse $ fst results
  where
    results = foldl applyFunction ([], as) bs
    applyFunction (zipped, []) _ = (zipped, [])
    applyFunction (zipped, x:xs) val = (f x val : zipped, xs)
