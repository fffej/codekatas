module Sudoku where

import Test.QuickCheck 
import Test.Hspec

import Data.Array
import Data.Char (isDigit)

data Cell = Unknown [Int]
          | Known Int
            deriving (Show,Eq)

type Grid = Array (Int,Int) Cell

buildGrid :: String -> Grid
buildGrid s = listArray bounds (map toCell s)
  where
    bounds :: ((Int,Int),(Int,Int))
    bounds = ((0,0),(8,8))

at :: Grid -> (Int,Int) -> Cell
at g p = g ! p

toCell :: Char -> Cell
toCell c
  | isDigit c = Known (read [c])
  | otherwise = Unknown [1..9]

row :: Grid -> (Int,Int) -> [Cell]
row g (r,c) = undefined

col :: Grid -> (Int,Int) -> [Cell]
col g (r,c) = undefined

subgrid :: Grid -> (Int,Int) -> [Cell]
subgrid g (r,c) = undefined

solve :: Grid -> Grid
solve = undefined

veryEasy :: String
veryEasy = "6185___2_" ++
           "_5__17_63" ++
           "__326__95" ++
           "8_1_5_74_" ++
           "5__6_13_9" ++
           "_9782_5__" ++
           "286_45___" ++
           "1____26_4" ++
           "_4_1_6258"

main = hspec $ do
  describe "Sudoku" $ do
    it "can read from string (1)" $ do
      at (buildGrid veryEasy) (0,0) `shouldBe` (Known 6)
    it "can read from string (2)" $ do
      at (buildGrid veryEasy) (8,8) `shouldBe` (Known 8)
    it "can read from string (3)" $ do
      at (buildGrid veryEasy) (0,7) `shouldBe` (Known 2)
