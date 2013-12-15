module GameOfLife where

import Test.Hspec
import Test.QuickCheck

data CellState = Live | Dead deriving (Show,Eq)

nextState :: CellState -> Int -> CellState
nextState Live n
  | n < 2 || n > 3 = Dead
  | otherwise = Live

main :: IO ()
main = hspec $ do
  describe "Game of life" $ do
    it "A live cell fewer than 2 live neighbours, dies" $ do 
      all (\n -> nextState Live n == Dead) [0,1]
    it "A live cell with 2 or 3 live neighbours live on" $ do
      all (\n -> nextState Live n == Live) [2,3]
    it "A live cell with more than 3 live neighbours dies" $ do
      all (\n -> nextState Live n == Dead) [4..9]
