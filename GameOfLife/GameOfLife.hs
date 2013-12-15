module GameOfLife where

import Test.Hspec
import Test.QuickCheck

data CellState = Live | Dead deriving (Show,Eq)

nextState :: CellState -> Int -> CellState
nextState Live 2 = Dead

main :: IO ()
main = hspec $ do
  describe "Game of life" $ do
    it "A live cell with 2 or less neighbours, dies" $ do 
      all (\n -> nextState Live n == Dead) [0,1,2]
    

