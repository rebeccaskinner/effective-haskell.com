module EffectiveHaskell.Exercises.Chapter1.ManualCurryingSpec where
import EffectiveHaskell.Exercises.Chapter1.ManualCurrying
import Test.Hspec

spec :: Spec
spec = do
  testExampleCurry
  testExampleUncurry

testExampleCurry :: Spec
testExampleCurry = do
  describe "exampleCurry" $ do
    it "curries a function of two arguments" $ do
      let f = exampleCurry $ \(a,b) -> a + b
      f 1 2 `shouldBe` 3
    it "curries a function of three arguments" $ do
      let f = exampleCurry $ \(a,b) c -> a + b + c
      f 1 2 3 `shouldBe` 6
    it "works follows the examples from the first hint" $ do
      let
        addTuple tuple = fst tuple + snd tuple
        addTwo = exampleCurry addTuple
      addTuple (1,2) `shouldBe` 3
      exampleCurry addTuple 1 2 `shouldBe` 3
      addTwo 1 2 `shouldBe` 3
      addTwo 3 4 `shouldBe` 7

testExampleUncurry :: Spec
testExampleUncurry = do
  describe "exampleUncurry" $ do
    it "uncurries a function of two arguments" $ do
      let f = exampleUncurry $ \a b -> a + b
      f (1,2) `shouldBe` 3
    it "uncurries a function of three arguments" $ do
      let f = exampleUncurry $ \a b c -> a + b + c
      f (1,2) 3 `shouldBe` 6
