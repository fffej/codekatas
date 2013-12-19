module StringCalculator where

import Data.List.Split

import Test.Hspec
import Test.QuickCheck

customDelimter :: String -> (Char -> Bool)
customDelimter xs
  | (take 2 xs) == "//" = \x -> x == xs !! 2
  |  otherwise          = \x -> (x == ',' || x == '\n')

parse :: String -> String
parse xs
  | (take 2 xs) == "//" = drop 4 xs
  | otherwise           = xs

add :: String -> Either String Int
add xs
  | any (< 0) nums  = Left "Negatives not allowed"
  | otherwise       = Right (sum nums)
  where
    nums = map parseInt (splitWhen delimFn toParse)
    delimFn = customDelimter xs
    toParse = parse xs
    
parseInt :: String -> Int
parseInt = read

main :: IO () 
main = hspec $ do
  describe "String calculator" $ do
    it "single number is identity" $ do
      add "1" `shouldBe` (Right 1)
    it "numbers separated by , added" $ do
      add "1,2" `shouldBe` (Right 3)
    it "any number of numbers" $ do
      add "1,2,3,4,5" `shouldBe` (Right 15)
    it ", and \\n are allowed" $ do
      add "1\n2,3" `shouldBe` (Right 6)
    it "supports custom delimiters" $ do
      add "//:\n1:2" `shouldBe` (Right 3)
    it "negatives aren't allowed" $ do
      add "-1" `shouldBe` (Left "Negatives not allowed")

