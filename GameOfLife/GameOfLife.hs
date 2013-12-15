module GameOfLife where

import Test.Hspec
import Test.QuickCheck

import Data.List ((\\))
import Data.Map (Map)
import qualified Data.Map as M


data CellState = Live | Dead deriving (Show,Eq)

data Rules = Rules
             {
               live :: [Int]
             , dead :: [Int]
             } deriving (Show,Eq)

defaultRules :: Rules
defaultRules = Rules
                 {
                   live = [2,3]
                 , dead = [3]
                 }

tick :: Rules -> CellState -> Int -> CellState
tick rules Live n = if (any (== n) (live rules)) then Live else Dead
tick rules Dead n = if (any (== n) (dead rules)) then Live else Dead


nextState :: CellState -> Int -> CellState
nextState = tick defaultRules

main :: IO ()
main = hspec $ do
  describe "Game of life" $ do
    it "A live cell with anything other than 2 or 3 neighbours, dies" $ do 
      all (\n -> nextState Live n == Dead) ([0..9] \\ [2,3])
    it "A live cell with 2 or 3 live neighbours live on" $ do
      all (\n -> nextState Live n == Live) [2,3]
    it "A dead cell with 3 neighbours becomes alive" $ do
      nextState Dead 3 == Live
    it "A dead cell with anything else stays dead" $ do
      all (\n -> nextState Dead n == Dead) ([0..9] \\ [3])
