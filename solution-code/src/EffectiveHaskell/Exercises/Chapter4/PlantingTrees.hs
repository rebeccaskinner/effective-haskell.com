{-# LANGUAGE DerivingStrategies #-}
module EffectiveHaskell.Exercises.Chapter4.PlantingTrees where

data BinaryTree a = Leaf | Branch (BinaryTree a) a (BinaryTree a)
  deriving stock (Eq, Show)

showStringTree :: BinaryTree String -> String
showStringTree = mergeWith "," . sortTree
  where
    mergeWith c (x:y:zs) = x <> c <> mergeWith c (y:zs)
    mergeWith c xs = concat xs
    sortTree Leaf = []
    sortTree (Branch l a r) = sortTree l <> [a] <> sortTree r

showTree :: BinaryTree Int -> BinaryTree String
showTree Leaf = Leaf
showTree (Branch l a r) = Branch (showTree l) (show a) (showTree r)

addElementToIntTree :: BinaryTree Int -> Int -> BinaryTree Int
addElementToIntTree tree n =
  case tree of
    Leaf -> Branch Leaf n Leaf
    Branch l a r
      | n > a -> Branch l a (addElementToIntTree r n)
      | n < a -> Branch (addElementToIntTree l n) a r
      | otherwise -> Branch l a r

doesIntExist :: BinaryTree Int -> Int -> Bool
doesIntExist Leaf _ = False
doesIntExist (Branch l a r) n
  | n > a = doesIntExist r n
  | n < a = doesIntExist l n
  | otherwise = True
