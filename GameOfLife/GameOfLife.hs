module GameOfLife where

import Test.Hspec
import Test.QuickCheck

data CellState = Live | Dead deriving (Show,Eq)

nextState :: CellState -> Int -> CellState
nextState Live n
  | n < 2 = Dead
  | otherwise = undefined

main :: IO ()
main = hspec $ do
  describe "Game of life" $ do
    it "A live cell fewer than 2 live neighbours, dies" $ do 
      all (\n -> nextState Live n == Dead) [0,1]
    

