module Problems where

import Test.Hspec
import Test.QuickCheck

insertAt :: a -> [a] -> Int -> [a]
insertAt = undefined

main = hspec $ do
  describe "99 problems" $ do
    it "should insert at" $ do
      insertAt 'X' "abcd" 2 `shouldBe` "aXbcd"
