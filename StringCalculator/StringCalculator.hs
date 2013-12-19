module StringCalculator where

import Data.List.Split

import Test.Hspec
import Test.QuickCheck

customDelimter :: String -> (Char -> Bool)
customDelimter _ = \x -> (x == ',' || x == '\n')

add :: String -> Int
add xs
  | ',' `elem` xs = sum (map parseInt (splitWhen delimFn xs))
  | otherwise = parseInt xs
  where
    delimFn = customDelimter xs


parseInt :: String -> Int
parseInt = read

main :: IO () 
main = hspec $ do
  describe "String calculator" $ do
    it "single number is identity" $ do
      add "1" `shouldBe` 1
    it "numbers separated by , added" $ do
      add "1,2" `shouldBe` 3
    it "any number of numbers" $ do
      add "1,2,3,4,5" `shouldBe` 15
    it ", and \n are allowed" $ do
      add "1\n2,3" `shouldBe` 6
    it "supports custom delimiters" $ do
      add "//:\n1:2" `shouldBe` 3
