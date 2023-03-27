module EffectiveHaskell.Exercises.Chapter3.FunctionsByType where
import Prelude hiding (concat, id)

swap :: (a,b) -> (b,a)
swap input =
  let
    newFirstElem = snd input
    newSecondElem = fst input
  in (newFirstElem, newSecondElem)

-- swap (a,b) = (b,a)

concat :: [[a]] -> [a]
concat = foldr (<>) []
-- concat [] = []
-- concat (x:xs) = x <> concat xs

concatFoldl :: [[a]] -> [a]
concatFoldl = foldl (<>) []
 -- where go acc sublist = acc <> sublist

concatReverse :: [[a]] -> [a]
concatReverse = reverse . foldr (<>) []

concatLongest :: [[a]] -> [a]
concatLongest = foldr getLongest []
  where
    getLongest subList currentLongest
      | length subList > length currentLongest = subList
      | otherwise = currentLongest

concatReturnFirst :: [[a]] -> [a]
concatReturnFirst [] = []
concatReturnFirst (x:_) = x

id :: a -> a
id a = a
