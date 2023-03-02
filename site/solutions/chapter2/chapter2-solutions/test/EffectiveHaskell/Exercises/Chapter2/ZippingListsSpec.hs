{-# LANGUAGE TypeApplications #-}
module EffectiveHaskell.Exercises.Chapter2.ZippingListsSpec where
import EffectiveHaskell.Exercises.Chapter2.ZippingLists
import Test.Hspec

spec :: Spec
spec = do
  describe "exampleZipWith" $ do
    it "returns an empty list if both lists are empty" $ do
      exampleZipWith @Int @Int @(Int,Int) (,) [] [] `shouldBe` []
    it "returns an empty list if one list is empty" $ do
      exampleZipWith @Int @Int @(Int,Int) (,) [1,2,3] [] `shouldBe` []
      exampleZipWith @Int @Int @(Int,Int) (,) [] [4,5,6] `shouldBe` []
    it "returns only as many elements as the shortest list" $ do
      exampleZipWith @Int @Int @(Int,Int) (,) [1,2,3,4,5] [1,2,3] `shouldBe` [(1,1),(2,2),(3,3)]
      exampleZipWith @Int @Int @(Int,Int) (,) [1,2,3] [1,2,3,4,5] `shouldBe` [(1,1),(2,2),(3,3)]
    it "returns all elements when the lists are the same length" $ do
      exampleZipWith @Int @Int @(Int,Int) (,) [1,2,3] [1,2,3] `shouldBe` [(1,1),(2,2),(3,3)]

  describe "exampleZipWith'" $ do
    it "returns an empty list if both lists are empty" $ do
      exampleZipWith' @Int @Int @(Int,Int) (,) [] [] `shouldBe` []
    it "returns an empty list if one list is empty" $ do
      exampleZipWith' @Int @Int @(Int,Int) (,) [1,2,3] [] `shouldBe` []
      exampleZipWith' @Int @Int @(Int,Int) (,) [] [4,5,6] `shouldBe` []
    it "returns only as many elements as the shortest list" $ do
      exampleZipWith' @Int @Int @(Int,Int) (,) [1,2,3,4,5] [1,2,3] `shouldBe` [(1,1),(2,2),(3,3)]
      exampleZipWith' @Int @Int @(Int,Int) (,) [1,2,3] [1,2,3,4,5] `shouldBe` [(1,1),(2,2),(3,3)]
    it "returns all elements when the lists are the same length" $ do
      exampleZipWith' @Int @Int @(Int,Int) (,) [1,2,3] [1,2,3] `shouldBe` [(1,1),(2,2),(3,3)]

  describe "exampleZipWithComprehension" $ do
    it "returns an empty list if both lists are empty" $ do
      exampleZipWithComprehension @Int @Int @(Int,Int) (,) [] [] `shouldBe` []
    it "returns an empty list if one list is empty" $ do
      exampleZipWithComprehension @Int @Int @(Int,Int) (,) [1,2,3] [] `shouldBe` []
      exampleZipWithComprehension @Int @Int @(Int,Int) (,) [] [4,5,6] `shouldBe` []
    it "returns only as many elements as the shortest list" $ do
      exampleZipWithComprehension @Int @Int @(Int,Int) (,) [1,2,3,4,5] [1,2,3] `shouldBe` [(1,1),(2,2),(3,3)]
      exampleZipWithComprehension @Int @Int @(Int,Int) (,) [1,2,3] [1,2,3,4,5] `shouldBe` [(1,1),(2,2),(3,3)]
    it "returns all elements when the lists are the same length" $ do
      exampleZipWithComprehension @Int @Int @(Int,Int) (,) [1,2,3] [1,2,3] `shouldBe` [(1,1),(2,2),(3,3)]

  describe "exampleZipWithComprehensionBad" $ do
    it "returns an empty list if both lists are empty" $ do
      exampleZipWithComprehensionBad @Int @Int @(Int,Int) (,) [] [] `shouldBe` []
    it "returns an empty list if one list is empty" $ do
      exampleZipWithComprehensionBad @Int @Int @(Int,Int) (,) [1,2,3] [] `shouldBe` []
      exampleZipWithComprehensionBad @Int @Int @(Int,Int) (,) [] [4,5,6] `shouldBe` []
    it "returns the product of both lists" $ do
      let
        list = [1,2,3]
        expected = [(1,1),(1,2),(1,3),(2,1),(2,2),(2,3),(3,1),(3,2),(3,3)]
      exampleZipWithComprehensionBad @Int @Int @(Int,Int) (,) list list `shouldBe` expected

  describe "zipWithFoldr" $ do
    it "returns an empty list if both lists are empty" $ do
      zipWithFoldr @Int @Int @(Int,Int) (,) [] [] `shouldBe` []
    it "returns an empty list if one list is empty" $ do
      zipWithFoldr @Int @Int @(Int,Int) (,) [1,2,3] [] `shouldBe` []
      zipWithFoldr @Int @Int @(Int,Int) (,) [] [4,5,6] `shouldBe` []
    it "returns only as many elements as the shortest list" $ do
      zipWithFoldr @Int @Int @(Int,Int) (,) [1,2,3,4,5] [1,2,3] `shouldBe` [(1,1),(2,2),(3,3)]
      zipWithFoldr @Int @Int @(Int,Int) (,) [1,2,3] [1,2,3,4,5] `shouldBe` [(1,1),(2,2),(3,3)]
    it "returns all elements when the lists are the same length" $ do
      zipWithFoldr @Int @Int @(Int,Int) (,) [1,2,3] [1,2,3] `shouldBe` [(1,1),(2,2),(3,3)]

  describe "zipWithFoldl" $ do
    it "returns an empty list if both lists are empty" $ do
      zipWithFoldl @Int @Int @(Int,Int) (,) [] [] `shouldBe` []
    it "returns an empty list if one list is empty" $ do
      zipWithFoldl @Int @Int @(Int,Int) (,) [1,2,3] [] `shouldBe` []
      zipWithFoldl @Int @Int @(Int,Int) (,) [] [4,5,6] `shouldBe` []
    it "returns only as many elements as the shortest list" $ do
      zipWithFoldl @Int @Int @(Int,Int) (,) [1,2,3,4,5] [1,2,3] `shouldBe` [(1,1),(2,2),(3,3)]
      zipWithFoldl @Int @Int @(Int,Int) (,) [1,2,3] [1,2,3,4,5] `shouldBe` [(1,1),(2,2),(3,3)]
    it "returns all elements when the lists are the same length" $ do
      zipWithFoldl @Int @Int @(Int,Int) (,) [1,2,3] [1,2,3] `shouldBe` [(1,1),(2,2),(3,3)]

  describe "zipWithFoldl'" $ do
    it "returns an empty list if both lists are empty" $ do
      zipWithFoldl' @Int @Int @(Int,Int) (,) [] [] `shouldBe` []
    it "returns an empty list if one list is empty" $ do
      zipWithFoldl' @Int @Int @(Int,Int) (,) [1,2,3] [] `shouldBe` []
      zipWithFoldl' @Int @Int @(Int,Int) (,) [] [4,5,6] `shouldBe` []
    it "returns only as many elements as the shortest list" $ do
      zipWithFoldl' @Int @Int @(Int,Int) (,) [1,2,3,4,5] [1,2,3] `shouldBe` [(1,1),(2,2),(3,3)]
      zipWithFoldl' @Int @Int @(Int,Int) (,) [1,2,3] [1,2,3,4,5] `shouldBe` [(1,1),(2,2),(3,3)]
    it "returns all elements when the lists are the same length" $ do
      zipWithFoldl' @Int @Int @(Int,Int) (,) [1,2,3] [1,2,3] `shouldBe` [(1,1),(2,2),(3,3)]

  describe "prelude zipWith" $ do
    it "returns an empty list if both lists are empty" $ do
      zipWith @Int @Int @(Int,Int) (,) [] [] `shouldBe` []
    it "returns an empty list if one list is empty" $ do
      zipWith @Int @Int @(Int,Int) (,) [1,2,3] [] `shouldBe` []
      zipWith @Int @Int @(Int,Int) (,) [] [4,5,6] `shouldBe` []
    it "returns only as many elements as the shortest list" $ do
      zipWith @Int @Int @(Int,Int) (,) [1,2,3,4,5] [1,2,3] `shouldBe` [(1,1),(2,2),(3,3)]
      zipWith @Int @Int @(Int,Int) (,) [1,2,3] [1,2,3,4,5] `shouldBe` [(1,1),(2,2),(3,3)]
    it "returns all elements when the lists are the same length" $ do
      zipWith @Int @Int @(Int,Int) (,) [1,2,3] [1,2,3] `shouldBe` [(1,1),(2,2),(3,3)]

  describe "prelude exampleZipWithComprehensionIndex" $ do
    it "returns an empty list if both lists are empty" $ do
      exampleZipWithComprehensionIndex @Int @Int @(Int,Int) (,) [] [] `shouldBe` []
    it "returns an empty list if one list is empty" $ do
      exampleZipWithComprehensionIndex @Int @Int @(Int,Int) (,) [1,2,3] [] `shouldBe` []
      exampleZipWithComprehensionIndex @Int @Int @(Int,Int) (,) [] [4,5,6] `shouldBe` []
    it "returns only as many elements as the shortest list" $ do
      exampleZipWithComprehensionIndex @Int @Int @(Int,Int) (,) [1,2,3,4,5] [1,2,3] `shouldBe` [(1,1),(2,2),(3,3)]
      exampleZipWithComprehensionIndex @Int @Int @(Int,Int) (,) [1,2,3] [1,2,3,4,5] `shouldBe` [(1,1),(2,2),(3,3)]
    it "returns all elements when the lists are the same length" $ do
      exampleZipWithComprehensionIndex @Int @Int @(Int,Int) (,) [1,2,3] [1,2,3] `shouldBe` [(1,1),(2,2),(3,3)]
