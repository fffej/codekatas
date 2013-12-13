module BowlingGame where

import Test.Hspec
import Test.QuickCheck

score :: [Int] -> Int
score pins
  | null pins = 0
  | isHalfStrike = 10 + (head rest) + score rest
  | otherwise = sum frame + score rest
  where
    frame = take 2 pins
    isHalfStrike = sum frame == 10
    rest = drop 2 pins

main :: IO ()
main = hspec $ do
  describe "Gutter game" $ do
    it "rolling 20 zeros scores 0" $ do
      score (replicate 20 0) `shouldBe` 0
    it "rolling 20 single pins scores 20" $ do
      score (replicate 20 1) `shouldBe` 20
    it "half strikes scored correctly" $ do
      score ([5,5,2] ++ replicate 17 0) `shouldBe` (5 + 5 + 2 + 2)
    it "strikes score correctly" $ do
      score ([10,2,3] ++ replicate 16 0) `shouldBe` (10 + 2 + 3 + 2 + 3)
