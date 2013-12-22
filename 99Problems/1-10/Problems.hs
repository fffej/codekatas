module Problems where

import Test.Hspec
import Test.QuickCheck

myLast :: [a] -> a
myLast (x:[]) = x
myLast (_:xs) = myLast xs

myButLast :: [a] -> a
myButLast (x:y:[]) = x
myButLast (x:xs)   = myButLast xs

elementAt :: [a] -> Int -> a
elementAt = undefined

main :: IO ()
main = hspec $ do
  describe "List functions" $ do
    it "should find the last element of the list" $ do
      myLast [1,2,3,4] `shouldBe` 4
    it "last of an empty list should be an error" $ do
      myLast [] `shouldThrow` anyException
    it "should find last but one element of the list" $ do
      myButLast [1,2,3,4] `shouldBe` 3
    it "element at finds the nth element of the list" $ do
      elementAt [1,2,3] 2 `shouldBe` 2
