module StringCalculator where

import Test.Hspec
import Test.QuickCheck

add :: String -> Int
add = read

main :: IO () 
main = hspec $ do
  describe "String calculator" $ do
    it "single number is identity" $ do
      add "1" `shouldBe` 1
