module BowlingGame where

import Test.Hspec
import Test.QuickCheck

score :: [Int] -> Int
score pins = score' pins 0

score' :: [Int] -> Int -> Int
score' pins frame
  | null pins = 0
  | isStrike = 10 + sum (take 2 rest) + score' rest (frame + 1)
  | isHalfStrike = 10 + (head rest) + score' rest (frame + 1)
  | otherwise = sum (take 2 pins) + score' rest (frame + 1)
  where
    isStrike = head pins == 10
    isHalfStrike = sum (take 2 pins) == 10
    rest = drop (if isStrike then 1 else 2) pins
    
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
    it "scores the perfect game" $ do
      score (replicate 12 10) `shouldBe` 300 
