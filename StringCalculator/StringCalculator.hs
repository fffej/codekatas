module StringCalculator where

import Data.List.Split

import Test.Hspec
import Test.QuickCheck

add :: String -> Int
add xs
  | ',' `elem` xs = sum (map parseInt (splitOn "," xs))
  | otherwise = parseInt xs

parseInt :: String -> Int
parseInt = read

main :: IO () 
main = hspec $ do
  describe "String calculator" $ do
    it "single number is identity" $ do
      add "1" `shouldBe` 1
    it "numbers separated by , added" $ do
      add "1,2" `shouldBe` 3
