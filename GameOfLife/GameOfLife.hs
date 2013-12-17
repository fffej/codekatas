module GameOfLife where

import Test.Hspec
import Test.QuickCheck

import Data.Array
import Data.List ((\\))
import Data.Map (Map)
import qualified Data.Map as M

data CellState = Live | Dead deriving (Ord,Show,Eq)

type Rules = Map CellState [Int]

type Point = (Int,Int)

type Grid = Array Point CellState

mkGrid :: Int -> Int -> Grid
mkGrid width height = listArray ((0,0),(width,height)) (repeat Dead)

cellAt :: Grid -> Point -> CellState
cellAt g p = g ! p

neighbours :: Point -> [Point]
neighbours (x,y) = [(x+dx,y+dy) | dx <- [-1,0,1], dy <- [-1,0,1] ]

defaultRules :: Rules
defaultRules = M.fromList [(Live, [2,3]), (Dead, [3])]

tick :: Rules -> CellState -> Int -> CellState
tick rules state n = if (any (== n) (rules M.! state)) then Live else Dead

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
  describe "Grid" $ do
    it "each cell has 9 neighbours" $ do
      length (neighbours (1,1)) == 9
    it "grids are initially all dead" $ do
      cellAt (mkGrid 3 3) (1,1) == Dead
      
