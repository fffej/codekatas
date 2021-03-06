module GameOfLife where
    
import Test.Hspec
import Test.QuickCheck

import Data.Set (Set)
import qualified Data.Set as S

import Data.List ((\\))

import Data.Array.Unboxed
import Data.Word (Word8,Word16)
import Data.ByteString (ByteString,pack)
import Codec.BMP

import System.Random
import Control.Monad

type CellState = Bool

data Rules = Rules
             {
               live :: [Int]
             , dead :: [Int]
             } deriving (Show,Eq)

type Point = (Int,Int)

data Grid = Grid
            {
              liveCell :: Set Point
            , dimensions :: (Int,Int)
            } deriving (Eq,Show)

gridPoints :: Grid -> [Point]
gridPoints grid = [(x,y) | x <- [0..w], y <- [0..h]]
  where
    (w,h) = dimensions grid

mkGrid :: Int -> Int -> Grid
mkGrid w h = Grid S.empty (w,h)

mkGrid' :: (Int,Int) -> [Point] -> Grid
mkGrid' bounds points = Grid (S.fromList points) bounds

mkGridFromGrid :: Grid -> [Point] -> Grid
mkGridFromGrid g p = mkGrid' (dimensions g) p

liveCellAt :: Grid -> Point -> Bool
liveCellAt (Grid s _) p = S.member p s
 
numOfLiveNeighbours :: Grid -> Point -> Int
numOfLiveNeighbours g p = length (filter id (map (liveCellAt g) n))
  where
    n = map (bound (dimensions g)) $ neighbours p

neighbours :: Point -> [Point]
neighbours (x,y) = [(x+dx,y+dy) | dx <- [-1,0,1], dy <- [-1,0,1] ] \\ [(x,y)]

bound :: Point -> Point -> Point
bound (x,y) (mx,my) = (dx,dy)
  where
    dx
      | x > mx = 0
      | x < 0 = mx
      | otherwise = x
    dy
      | y > my = 0
      | y < 0 = my
      | otherwise = y

defaultRules :: Rules
defaultRules = Rules [2,3] [3]

tickCell :: Rules -> CellState -> Int -> CellState
tickCell rules True = tickCell' (live rules)
tickCell rules False = tickCell' (dead rules)

tickCell' :: [Int] -> Int -> CellState
tickCell' xs s = s `elem` xs

nextState :: CellState -> Int -> CellState
nextState = tickCell defaultRules

run :: Rules -> Grid -> [Grid]
run rules = iterate step'
  where
    step' = step rules

step :: Rules -> Grid -> Grid
step rules grid = mkGridFromGrid grid newLiveCells
  where
    points :: [Point]
    points = gridPoints grid

    cellStatus :: [(Point,CellState)]
    cellStatus = map (\p -> (p,liveCellAt grid p)) points

    neighbourCount :: [Int]
    neighbourCount = map (\(p,_) -> numOfLiveNeighbours grid p) cellStatus

    ps :: [(Point,CellState,Int)]
    ps = zipWith (\(p,c) n -> (p,c,n)) cellStatus neighbourCount

    newStates :: [(Point,CellState)]
    newStates = map (\(p,c,n) -> (p,tickCell rules c n)) ps

    newLiveCells :: [Point]
    newLiveCells = map fst (filter snd newStates)

gridToBMP :: Grid -> ByteString
gridToBMP grid = pack $ concatMap (\y -> (if y then white else black)) cellStatus
  where
    white :: [Word8]
    white = [255,255,255,0]
    black :: [Word8]
    black = [0,0,0,0]
    points = gridPoints grid
    cellStatus = map (\p -> liveCellAt grid p) points

gridToImage :: Grid -> FilePath -> IO ()
gridToImage grid path =  writeBMP path bmp
  where
    (x,y) = dimensions grid
    bmp = packRGBA32ToBMP (x + 1) (y + 1) (gridToBMP grid)

randomGrid :: Int -> Int -> IO Grid
randomGrid w h = do
  x <- filterM (\x -> getStdRandom random) [(x,y) | x <- [0..w], y <- [0..h]]
  return (Grid (S.fromList x) (w,h))


writeFiles :: FilePath -> IO ()
writeFiles path= do
  grid <- randomGrid 64 64
  let x = take 100 $ run defaultRules grid
      y = [path ++ show c ++ ".bmp" | c <- [0 .. ]]
      z = zip x y 
  sequence_ (map (\(g,p) -> gridToImage g p) z)

test :: IO ()
test = hspec $ do
  describe "Game of life" $ do
    it "A live cell with anything other than 2 or 3 neighbours, dies" $ do 
      all (\n -> nextState True n == False) ([0..9] \\ [2,3])
    it "A live cell with 2 or 3 live neighbours live on" $ do
      all (\n -> nextState True n) [2,3]
    it "A dead cell with 3 neighbours becomes alive" $ do
      nextState False 3
    it "A dead cell with anything else stays dead" $ do
      all (\n -> not (nextState False n)) ([0..9] \\ [3])
  describe "Grid" $ do
    it "each cell has 8 neighbours (not including itself!" $ do
      length (neighbours (1,1)) == 8
    it "cells are bounded" $ do
      all (\(x,y) -> x >= 0 && x < 10) (map (bound (10,10)) (neighbours (0,0)))
    it "grids are initially all dead" $ do
      all (\x -> liveCellAt (mkGrid 3 3) x == False) [(0,0),(1,1)]
    it "live neighbours can be retrieved" $ do
      numOfLiveNeighbours (mkGrid 3 3) (0,0) == 0
    it "live neighbours count is correct" $ do
      numOfLiveNeighbours (mkGrid' (3,3) [(x,y) | x <- [0..3], y <- [0..3]]) (0,0) `shouldBe` 8
    it "an empty grid translates to an empty grid" $ do
      step defaultRules (mkGrid 3 3) == mkGrid 3 3
