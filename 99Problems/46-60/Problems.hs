module Problems where

import Test.Hspec
import Test.QuickCheck

and'  = undefined
or'   = undefined

table :: (Bool -> Bool -> Bool) -> [(Bool,Bool,Bool)]
table = undefined

main :: IO ()
main = hspec $ do
  describe "Logic and codes" $ do
    it "should create truth table" $ do
      table (\a b -> (and' a (or' a b))) `shouldBe` [(True,True,True)
                                                    ,(True,False,True)
                                                    ,(False,True,False)
                                                    ,(False,False,False)]
