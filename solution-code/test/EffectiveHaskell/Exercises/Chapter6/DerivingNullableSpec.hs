{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module EffectiveHaskell.Exercises.Chapter6.DerivingNullableSpec where

import EffectiveHaskell.Exercises.Chapter6.DerivingNullable
import Prelude hiding (null)
import Test.Hspec

data TestNullableData = TestNull | TestJust Int
  deriving stock (Eq, Show)

instance Nullable TestNullableData where
  isNull TestNull = True
  isNull (TestJust _) = False
  null = TestNull

spec :: Spec
spec = do
  testTransitiveNullable
  testBasicNullable
  testOptionalString
  testOptionalNonEmptyString

testTransitiveNullable :: Spec
testTransitiveNullable = do
  describe "isNull" $ do
    it "returns true with Nothing" $ do
      isNull (TransitiveNullable @TestNullableData Nothing) `shouldBe` True
    it "returns true with a value null" $ do
      isNull (TransitiveNullable (Just TestNull)) `shouldBe` True
    it "returns false with a singleton list of a non-null value" $ do
      isNull (TransitiveNullable (Just (TestJust 1))) `shouldBe` False
  describe "null" $ do
    it "returns an empty list of TestNullableData" $ do
      null @(TransitiveNullable TestNullableData) `shouldBe` TransitiveNullable Nothing

testBasicNullable :: Spec
testBasicNullable = do
  describe "isNull" $ do
    it "returns true with a Nothing value" $ do
      isNull (BasicNullable @TestNullableData Nothing) `shouldBe` True
    it "returns false with a Just null value" $ do
      isNull (BasicNullable (Just TestNull)) `shouldBe` False
    it "returns false with a Just non-null value" $ do
      isNull (BasicNullable (Just (TestJust 1))) `shouldBe` False
  describe "null" $ do
    it "returns a Nothing value" $ do
      null @(BasicNullable TestNullableData) `shouldBe` BasicNullable Nothing

testOptionalString :: Spec
testOptionalString = do
  describe "isNull" $ do
    it "returns True when there is a Nothing value" $ do
      isNull (OptionalString Nothing) `shouldBe` True
    it "returns False when there is an empty string" $ do
      isNull (OptionalString (Just "")) `shouldBe` False
    it "returns False when there is a non-empty string" $ do
      isNull (OptionalString (Just "foo")) `shouldBe` False
  describe "null" $ do
    it "returns a Nothing value" $ do
      null `shouldBe` OptionalString Nothing

testOptionalNonEmptyString :: Spec
testOptionalNonEmptyString = do
  describe "isNull" $ do
    it "returns True when there is a Nothing value" $ do
      isNull (OptionalNonEmptyString Nothing) `shouldBe` True
    it "returns True when there is an empty string" $ do
      isNull (OptionalNonEmptyString (Just "")) `shouldBe` True
    it "returns False when there is a non-empty string" $ do
      isNull (OptionalString (Just "foo")) `shouldBe` False
  describe "null" $ do
    it "returns a Nothing value" $ do
      null `shouldBe` OptionalString Nothing
