module Sudoku where

import Test.QuickCheck 
import Test.Hspec

import Data.Array
import Data.Char (isDigit)
import Data.Maybe
import Data.List

data Cell = Unknown [Int]
          | Known Int
            deriving (Show,Eq)

knownValue :: Cell -> Maybe Int
knownValue (Known n) = Just n
knownValue _         = Nothing

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

row :: Grid -> Int -> [Cell]
row g r = map snd (filter (\((x,y),e) -> x == r) (assocs g))

col :: Grid -> Int -> [Cell]
col g c = map snd (filter (\((x,y),e) -> y == c) (assocs g))

subgrid :: Grid -> (Int,Int) -> [Cell]
subgrid g (r,c) = map snd (filter isInSubGrid (assocs g))
  where
    isInSubGrid ((x,y),_) = r `div` 3 == x `div` 3 &&
                            c `div` 3 == y `div` 3 

step :: Grid -> (Int,Int) -> Cell
step g p@(r,c) =  case value of
  (Known n) -> value
  (Unknown ps) -> Unknown (ps \\ surroundingCells)
  where
    value = at g p
    surroundingCells :: [Int]
    surroundingCells = known (row g r ++ col g c ++ subgrid g p)

solve :: Grid -> Grid
solve = undefined

known :: [Cell] -> [Int]
known = mapMaybe knownValue

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
    it "row 0" $ do
      known (row (buildGrid veryEasy) 0) `shouldBe` [6,1,8,5,2]
    it "col 1" $ do
      known (col (buildGrid veryEasy) 1) `shouldBe` [1,5,9,8,4]
    it "subgrid 7,7" $ do
      known (subgrid (buildGrid veryEasy) (7,7)) `shouldBe` [6,4,2,5,8]
    it "eliminates possibilities" $ do
      step (buildGrid veryEasy) (0,8) `shouldBe` (Known 7)
