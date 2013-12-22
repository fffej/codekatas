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

isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == myReverse xs

data NestedList a = Elem a
                  | List [NestedList a]
                    deriving (Show,Eq)

flatten :: NestedList a -> [a]
flatten (Elem a) = [a]

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
    it "checks for palindomes (positive)" $ do
      isPalindrome [1,2,3,2,1]
    it "checks for palindrome (negative)" $ do
      not (isPalindrome [1,2,3])
    it "flattens nested lists" $ do
      flatten (Elem 5) `shouldBe` [5]
    it "really does flatten lists" $ do
      flatten (List [List [Elem 5]]) `shouldBe` [5]
