module GameOfLife where

import Test.Hspec
import Test.QuickCheck

data CellState = Live | Dead deriving (Show,Eq)

nextState :: CellState -> Int -> CellState
nextState Live 2 = Dead

main :: IO ()
main = hspec $ do
  describe "Game of life" $ do
    it "A live cell with 2 or less neighbours, dies" $ property
      (\n -> n >=0 && n <=2 ==> nextState Live n == Dead)
    

