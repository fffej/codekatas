module Problems where

import Test.Hspec
import Test.QuickCheck

myLast :: [a] -> a
myLast (x:[]) = x
myLast (_:xs) = myLast xs

main :: IO ()
main = hspec $ do
  describe "List functions" $ do
    it "should find the last element of the list" $ do
      myLast [1,2,3,4] `shouldBe` 4
    it "last of an empty list should be an error" $ do
      myLast [] `shouldThrow` anyException
