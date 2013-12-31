module Sudoku where

import Test.QuickCheck
import Test.Hspec

import Data.Array

data Cell = Unknown [Int]
          | Fixed Int
            deriving (Show,Eq)

type Grid = Array (Int,Int) Cell

grid :: Grid
grid = listArray bounds (repeat $ Unknown [1..9])
  where
    bounds :: ((Int,Int),(Int,Int))
    bounds = ((0,0),(8,8))

row :: Grid -> (Int,Int) -> [Cell]
row g (r,c) = undefined

col :: Grid -> (Int,Int) -> [Cell]
col g (r,c) = undefined

subgrid :: Grid -> (Int,Int) -> [Cell]
subgrid g (r,c) = undefined

main = undefined
