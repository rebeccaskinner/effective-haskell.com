module EffectiveHaskell.Exercises.Chapter1.FactorialSpec where
import EffectiveHaskell.Exercises.Chapter1.Factorial
import Test.Hspec

spec :: Spec
spec = do
  describe "factorial examples" $ do
    it "works for factorial 1" $ do
      factorial 1 `shouldBe` 1
    it "works for factorial 3" $ do
      factorial 3 `shouldBe` 6
    it "works for factorial 5" $ do
      factorial 5 `shouldBe` 120
    it "works for factorial 10" $ do
      factorial 10 `shouldBe` 3628800
    it "works for factorial 25" $ do
      factorial 25 `shouldBe` 15511210043330985984000000
