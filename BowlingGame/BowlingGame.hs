module BowlingGame where

import Test.Hspec
import Test.QuickCheck

score :: [Int] -> Int
score rolls = sum rolls

main :: IO ()
main = hspec $ do
  describe "Gutter game" $ do
    it "rolling 20 zeros scores 0" $ do
      score (replicate 20 0) `shouldBe` 0
    it "rolling 20 single pins scores 20" $ do
      score (replicate 20 1) `shouldBe` 20
