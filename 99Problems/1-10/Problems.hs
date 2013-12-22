module Problems where

import Test.Hspec
import Test.QuickCheck

myLast = undefined

main :: IO ()
main = hspec $ do
  describe "List functions" $ do
    it "should find the last element of the list" $ do
      myLast [1,2,3,4] `shouldBe` 4
