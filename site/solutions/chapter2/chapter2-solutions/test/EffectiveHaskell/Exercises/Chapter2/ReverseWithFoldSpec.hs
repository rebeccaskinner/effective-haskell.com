module EffectiveHaskell.Exercises.Chapter2.ReverseWithFoldSpec where
import EffectiveHaskell.Exercises.Chapter2.ReverseWithFold
import Test.Hspec

spec :: Spec
spec = do
  describe "reverseLeft" $ do
    it "works for an empty list" $ do
      reverseLeft [] `shouldBe` []
    it "works for a singleton list" $ do
      reverseLeft [1] `shouldBe` [1]
    it "works for a longer list" $ do
      let list = [1..1000]
      reverseLeft list `shouldBe` reverse list
  describe "reverseRight" $ do
    it "works for an empty list" $ do
      reverseRight [] `shouldBe` []
    it "works for a singleton list" $ do
      reverseRight [1] `shouldBe` [1]
    it "works for a longer list" $ do
      let list = [1..1000]
      reverseRight list `shouldBe` reverse list
