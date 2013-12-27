module Problems where

import Test.Hspec
import Test.QuickCheck

import Data.List

infixl 3 `equ'`

and'  = (&&)
or'   = (||)
equ'  = (==)


table :: (Bool -> Bool -> Bool) -> [(Bool,Bool,Bool)]
table f = zipWith (\(x,y) z -> (x,y,z)) inputs outputs
  where
    inputs  = [(x,y) | x <- [True,False], y <- [True,False]]
    outputs = map (uncurry f) inputs

table2 = table

tablen :: Int -> ([Bool] -> Bool) -> [[Bool]]
tablen n f = zipWith (\i o -> i ++ [o]) ins out
  where
    ins = genTruthTable n
    out = map f ins

genTruthTable :: Int -> [[Bool]]
genTruthTable 1 = [[True],[False]]
genTruthTable n = map (True :) rest ++ map (False :) rest
  where
    rest = genTruthTable (n - 1)

gray :: Int -> [String]
gray 2 = ["00","01","11","10"]
gray n = map ('0' : ) x ++  map ('1' :) rx
  where
    x  = gray (n - 1)
    rx = reverse x

huffman = undefined
    
truthTable :: [[Bool]]
truthTable = [[True,  True,  True,  True]
             ,[True,  True,  False, False]
             ,[True,  False, True,  True]
             ,[True,  False, False, True]
             ,[False, True,  True,  True]
             ,[False, True,  False, True]
             ,[False, False, True,  True]
             ,[False, False, False, True]]

main :: IO ()
main = hspec $ do
  describe "Logic and codes" $ do
    it "should create truth table" $ do
      table (\a b -> (and' a (or' a b))) `shouldBe` [(True,True,True)
                                                    ,(True,False,True)
                                                    ,(False,True,False)
                                                    ,(False,False,False)]
    it "should allow infix operations" $ do
      table2 (\a b -> a `and'` (a `or'` not b)) `shouldBe` [(True,True,True)
                                                           ,(True,False,True)
                                                           ,(False,True,False)
                                                           ,(False,False,False)]
    it "should support arbitrary expressions" $ do
      tablen 3 (\[a,b,c] -> (a `and'` (b `or'` c)) `equ'` (a `and'` b `or'` a `and'` c)) `shouldBe` truthTable
    it "gray codes" $ do
      gray 3 `shouldBe` ["000","001","011","010","110","111","101","100"]
    it "huffman encoding" $ do
      huffman [('a',45),('b',13),('c',12),('d',16),('e',9), ('f',5)] `shouldBe` [('a',"0"),('b',"101"),('c',"100"),('d',"111"),('e',"1101"),('f',"1100")]
