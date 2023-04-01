{-# LANGUAGE DerivingStrategies #-}
module EffectiveHaskell.Exercises.Chapter4.PlantingTrees where
-- import Data.List (intercalate)

data BinaryTree a = Leaf | Branch (BinaryTree a) a (BinaryTree a)
  deriving stock (Eq, Show)

showStringNaive :: BinaryTree String -> String
showStringNaive Leaf = ""
showStringNaive (Branch l a r) =
  leftStr <> "," <> a <> "," <> rightStr
  where
    leftStr = showStringNaive l
    rightStr = showStringNaive r

showStringTree :: BinaryTree String -> String
showStringTree = intercalate "," . binaryTreeToList

intercalate :: [a] -> [[a]] -> [a]
intercalate a (x:y:ys) = x <> a <> intercalate a (y:ys)
intercalate _ rest = concat rest

binaryTreeToList :: BinaryTree a -> [a]
binaryTreeToList Leaf = []
binaryTreeToList (Branch l a r) = binaryTreeToList l <> [a] <> binaryTreeToList r

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
