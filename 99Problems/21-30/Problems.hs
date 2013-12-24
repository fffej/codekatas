module Problems where

import Test.Hspec
import Test.QuickCheck

insertAt :: a -> [a] -> Int -> [a]
insertAt e xs 1      = e:xs
insertAt e (x:xs) n  = x : insertAt e xs (n - 1)

main = hspec $ do
  describe "99 problems" $ do
    it "should insert at" $ do
      insertAt 'X' "abcd" 2 `shouldBe` "aXbcd"
