module MyPrelude.CollectionsSpec where

import MyPrelude
import Utils.Testing

spec_allPairs :: Spec
spec_allPairs = do
  it "returns an empty list for an empty list" do
    allPairs ([] :: [Int]) `shouldBe` []
  it "returns an empty list for a singleton list" do
    allPairs [1 :: Int] `shouldBe` []
  it "returns all pairs" do
    asSet (setFromList (allPairs [1 :: Int, 2, 3]))
      `shouldBe` setFromList [(1, 2), (1, 3), (2, 3)]

spec_allAnyOrderPairs :: Spec
spec_allAnyOrderPairs = do
  it "returns an empty list for an empty list" do
    allAnyOrderPairs ([] :: [Int]) `shouldBe` []
  it "returns an empty list for a singleton list" do
    allAnyOrderPairs [1 :: Int] `shouldBe` []
  it "returns all pairs" do
    asSet (setFromList (allAnyOrderPairs [1 :: Int, 2, 3]))
      `shouldBe` setFromList [(1, 2), (1, 3), (2, 3), (2, 1), (3, 1), (3, 2)]
