module Sudoku where

import Test.QuickCheck
import Test.Hspec

import Data.Array

data Cell = Unknown [Int]
          | Fixed Int
            deriving (Show,Eq)

grid = listArray bounds (repeat $ Unknown [1..9])
  where
    bounds :: ((Int,Int),(Int,Int))
    bounds = ((0,0),(8,8))

main = undefined
