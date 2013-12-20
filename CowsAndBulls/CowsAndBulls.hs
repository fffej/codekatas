module CowsAndBulls where

import Test.Hspec
import Test.QuickCheck

data Guess = Guess Int Int Int Int deriving (Show,Eq)

data Score = Score
             {
               cows :: Int
             , bulls :: Int
             } deriving (Show,Eq)

score :: Guess -> Guess -> Score
score = undefined

main :: IO ()
main = hspec $ do
  describe "Scoring" $ do
    it "Correct guess scores 4 bulls" $ do
      score (Guess 1 2 3 4) (Guess 1 2 3 4) `shouldBe` (Score 4 0)
