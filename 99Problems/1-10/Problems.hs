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
elementAt (x:_ ) 1 = x
elementAt (_:xs) n = elementAt xs (n - 1)

myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs 

myReverse :: [a] -> [a]
myReverse xs = go xs []
  where
    go xs ys
      | null xs   = ys
      | otherwise = go (tail xs) (head xs:ys)  

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
    it "finds the length of a list" $ do
      myLength "Hello, world!" `shouldBe` 13
    it "myReverse reverses a string" $ do
      myReverse [1,2,3,4] `shouldBe` [4,3,2,1]
