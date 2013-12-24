module Problems where

import Test.Hspec
import Test.QuickCheck

insertAt = undefined

main = hspec $ do
  describe "99 problems" $ do
    it "should insert at" $ do
      insertAt 'X' "abcd" `shouldBe` "aXbcd"
