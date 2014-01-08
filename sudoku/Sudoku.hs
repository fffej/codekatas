module Sudoku where

import Test.QuickCheck 
import Test.Hspec

import Data.Array hiding (bounds)
import Data.Char (isDigit)
import Data.Maybe
import Data.List

import Control.Monad

import Data.List.Split

data Cell = Unknown [Int]
          | Known Int
            deriving (Eq)

instance Show Cell where
  show (Known n)   = show n
  show (Unknown _) = "_"

knownValue :: Cell -> Maybe Int
knownValue (Known n) = Just n
knownValue _         = Nothing

type RawGrid = Array (Int,Int) Cell

data Grid = Grid RawGrid deriving (Eq)

instance Show Grid where
    show = display

cells :: Grid -> [((Int,Int),Cell)]
cells (Grid g) = assocs g

at :: Grid -> (Int,Int) -> Cell
at (Grid g) p = g ! p 

cellStates :: Grid -> [Cell]
cellStates g = map snd (cells g)

bounds :: ((Int,Int),(Int,Int))
bounds = ((0,0),(8,8))

buildRawGrid :: String -> RawGrid
buildRawGrid s = listArray bounds (map toCell s)

buildGrid :: String -> Maybe Grid
buildGrid s 
    | isValid g = Just (Grid g)
    | otherwise = Nothing
    where
      g = applyConstraints $ buildRawGrid s

display :: Grid -> String
display g = unlines $ chunksOf 9 $ concatMap show (cellStates g)

toCell :: Char -> Cell
toCell c
  | isDigit c = Known (read [c])
  | otherwise = Unknown [1..9]

row :: RawGrid -> Int -> [Cell]
row g r = map snd (filter (\((x,y),e) -> x == r) (assocs g))

col :: RawGrid -> Int -> [Cell]
col g c = map snd (filter (\((x,y),e) -> y == c) (assocs g))

subgrid :: RawGrid -> (Int,Int) -> [Cell]
subgrid g (r,c) = map snd (filter isInSubGrid (assocs g))
  where
    isInSubGrid ((x,y),_) = r `div` 3 == x `div` 3 &&
                            c `div` 3 == y `div` 3 

eliminatePossibilities :: RawGrid -> (Int,Int) -> Cell
eliminatePossibilities g p@(r,c) = eliminatePossibilities' (g ! p) surroundingCells
  where
    surroundingCells = known $ row g r ++ col g c ++ subgrid g p
    
eliminatePossibilities' :: Cell -> [Int] -> Cell
eliminatePossibilities' (Known x) _     = Known x
eliminatePossibilities' (Unknown ys) xs
  | length rest == 1  = Known (head rest)
  | otherwise         = Unknown rest
  where
    rest = ys \\ xs

applyConstraints :: RawGrid -> RawGrid
applyConstraints g 
    | next == g = g
    | otherwise = applyConstraints next 
    where
      next = array bounds (map (\(x,e) -> (x,eliminatePossibilities g x)) (assocs g))

known :: [Cell] -> [Int]
known = mapMaybe knownValue

isSolvedGrid :: Grid -> Bool
isSolvedGrid (Grid r) = isSolved r

isSolved :: RawGrid -> Bool
isSolved g = all isKnown (map snd (assocs g))
  where
    isKnown (Known _) = True
    isKnown _         = False

isValid :: RawGrid -> Bool
isValid raw = isSolved raw || all choiceRemains (map snd (assocs raw))
    where
      choiceRemains (Known _) = True
      choiceRemains (Unknown xs) = (not . null) xs

choices :: Cell -> [Cell]
choices k@(Known n) = [k]
choices (Unknown ns)= map Known ns

data GridNode = GridNode Grid [GridNode] deriving (Show,Eq)

buildGraph :: Grid -> GridNode
buildGraph g
  | isSolvedGrid g = GridNode g []
  | otherwise      = undefined

veryEasy :: String
veryEasy = "6185___2_" ++
           "_5__17_63" ++
           "__326__95" ++
           "8_1_5_74_" ++
           "5__6_13_9" ++
           "_9782_5__" ++
           "286_45___" ++
           "1____26_4" ++
           "_4_1_6258"

veryEasySolution :: String
veryEasySolution = "618593427\n" ++
                   "952417863\n" ++
                   "473268195\n" ++
                   "861359742\n" ++
                   "524671389\n" ++
                   "397824516\n" ++
                   "286745931\n" ++
                   "135982674\n" ++
                   "749136258\n"

veryHard :: String
veryHard = "___2___63" ++
           "3____54_1" ++
           "_______9_" ++
           "__1__398_" ++
           "___538___" ++
           "_3_______" ++
           "_263__5__" ++
           "5_37____8" ++
           "47___1___"

veryHardSolution :: String
veryHardSolution = "854219763\n" ++
                   "397865421\n" ++
                   "261473985\n" ++
                   "785126394\n" ++
                   "649538172\n" ++
                   "132947856\n" ++
                   "926384517\n" ++
                   "513792648\n" ++
                   "478651239\n"

invalidGrid :: String
invalidGrid = "6185___2_" ++
              "_5__17_63" ++
              "__326__95" ++
              "8_1_5_74_" ++
              "5__6913_9" ++ -- the middle 9 is invalid
              "_9782_5__" ++
              "286_45___" ++
              "1____26_4" ++
              "_4_1_6258"


main = hspec $ do
  describe "Sudoku" $ do
    it "row 0" $ do
      known (row (buildRawGrid veryEasy) 0) `shouldBe` [6,1,8,5,2]
    it "col 1" $ do
      known (col (buildRawGrid veryEasy) 1) `shouldBe` [1,5,9,8,4]
    it "subgrid 7,7" $ do
      known (subgrid (buildRawGrid veryEasy) (7,7)) `shouldBe` [6,4,2,5,8]
    it "eliminates possibilities" $ do
      eliminatePossibilities (buildRawGrid veryEasy) (0,8) `shouldBe` (Known 7)
    it "a grid is solved if all of the cells are known" $ do
      isSolved (buildRawGrid veryEasy) `shouldBe` False
    it "applyConstraints simple examples" $ do
      maybe "" display (buildGrid veryEasy) `shouldBe` veryEasySolution
    it "applyConstraints an example that requires back-tracking" $ do
      maybe "" display (buildGrid veryHard) `shouldBe` veryHardSolution
    it "will guess a constrained cell" $ do
      choices (Unknown [1..9]) `shouldBe` map Known [1..9]
    it "has a predicate to determine invalid state" $ do
      buildGrid invalidGrid `shouldBe` Nothing
    it "builds a graph for an easy solution" $ do
      liftM buildGraph (buildGrid veryEasy) `shouldBe` liftM buildGraph (buildGrid veryEasy) 
