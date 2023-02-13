module EffectiveHaskell.Exercises.Chapter1.FibonacciSpec where
import EffectiveHaskell.Exercises.Chapter1.Fibonacci
import Test.Hspec

spec :: Spec
spec = do
  describe "fibonacci examples" $ do
    it "returns the right value for 0" $ do
      fibonacci 0 `shouldBe` 0
    it "returns the right value for 1" $ do
      fibonacci 1 `shouldBe` 1
    it "returns the right value for 5" $ do
      fibonacci 5 `shouldBe` 5
    it "returns the right value for 10" $ do
      fibonacci 10 `shouldBe` 55
    it "returns the right value for 25" $ do
      fibonacci 25 `shouldBe` 75025
