module Problems where

import Test.Hspec
import Test.QuickCheck

insertAt :: a -> [a] -> Int -> [a]
insertAt e xs 1      = e:xs
insertAt e (x:xs) n  = x : insertAt e xs (n - 1)

range :: Int -> Int -> [Int]
range = undefined

main = hspec $ do
  describe "99 problems" $ do
    it "should insert at" $ do
      insertAt 'X' "abcd" 2 `shouldBe` "aXbcd"
    it "should define range" $ do
      range 4 9 `shouldBe` [4,5,6,7,8,9]
