module GameOfLife where

import Test.Hspec
import Test.QuickCheck

import Data.Map (Map)
import qualified Data.Map as M
import Data.List ((\\))

data CellState = Live | Dead deriving (Ord,Show,Eq)

data Rules = Rules
             {
               live :: [Int]
             , dead :: [Int]
             } deriving (Show,Eq)

type Point = (Int,Int)

type Grid = Map Point CellState

mkGrid :: Int -> Int -> Grid
mkGrid w h = M.fromList [((x,y),Dead) | x <- [0..w], y <- [0..h]]

cellAt :: Grid -> Point -> CellState
cellAt g p = g M.! p

liveNeighbours :: Grid -> Point -> Int
liveNeighbours g p = undefined

neighbours :: Point -> [Point]
neighbours (x,y) = [(x+dx,y+dy) | dx <- [-1,0,1], dy <- [-1,0,1] ]

bound :: Point -> Point -> Point
bound (x,y) (mx,my) = (dx,dy)
  where
    dx = if x > mx then 0 else (if x < 0 then mx else x)
    dy = if y > my then 0 else (if y < 0 then my else y)

defaultRules :: Rules
defaultRules = Rules [2,3] [3]

tickCell :: Rules -> CellState -> Int -> CellState
tickCell rules Live = tickCell' (live rules)
tickCell rules Dead = tickCell' (dead rules)

tickCell' :: [Int] -> Int -> CellState
tickCell' xs s = if (elem s xs) then Live else Dead

nextState :: CellState -> Int -> CellState
nextState = tickCell defaultRules

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
    it "cells are bounded" $ do
      all (\(x,y) -> x >= 0 && x < 10) (map (bound (10,10)) (neighbours (0,0)))
    it "grids are initially all dead" $ do
      all (\x -> cellAt (mkGrid 3 3) x == Dead) [(0,0),(1,1)]
    it "live neighbours can be retrieved" $ do
      liveNeighbours (mkGrid 3 3) (0,0) == 0
