module Problems where

import Test.Hspec
import Test.QuickCheck

import Data.List

and'  = (&&)
or'   = (||)

table :: (Bool -> Bool -> Bool) -> [(Bool,Bool,Bool)]
table f = zipWith (\(x,y) z -> (x,y,z)) inputs outputs
  where
    inputs  = [(x,y) | x <- [True,False], y <- [True,False]]
    outputs = map (uncurry f) inputs

main :: IO ()
main = hspec $ do
  describe "Logic and codes" $ do
    it "should create truth table" $ do
      table (\a b -> (and' a (or' a b))) `shouldBe` [(True,True,True)
                                                    ,(True,False,True)
                                                    ,(False,True,False)
                                                    ,(False,False,False)]
