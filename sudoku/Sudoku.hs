module Sudoku where

import Test.QuickCheck 
import Test.Hspec

import Data.Array
import Data.Char (isDigit)
import Data.Maybe
import Data.List

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

type Grid = Array (Int,Int) Cell

buildGrid :: String -> Grid
buildGrid s = listArray bounds (map toCell s)
  where
    bounds :: ((Int,Int),(Int,Int))
    bounds = ((0,0),(8,8))

display :: Grid -> String
display g = unlines $ chunksOf 9 $ concatMap show (elems g)

toCell :: Char -> Cell
toCell c
  | isDigit c = Known (read [c])
  | otherwise = Unknown [1..9]

row :: Grid -> Int -> [Cell]
row g r = map snd (filter (\((x,y),e) -> x == r) (assocs g))

col :: Grid -> Int -> [Cell]
col g c = map snd (filter (\((x,y),e) -> y == c) (assocs g))

subgrid :: Grid -> (Int,Int) -> [Cell]
subgrid g (r,c) = map snd (filter isInSubGrid (assocs g))
  where
    isInSubGrid ((x,y),_) = r `div` 3 == x `div` 3 &&
                            c `div` 3 == y `div` 3 

stepCell :: Grid -> (Int,Int) -> Cell
stepCell g p@(r,c) = stepCell' (g ! p) surroundingCells
  where
    surroundingCells = known $ row g r ++ col g c ++ subgrid g p
    
stepCell' :: Cell -> [Int] -> Cell
stepCell' (Known x) _     = Known x
stepCell' (Unknown ys) xs
  | length rest == 1  = Known (head rest)
  | otherwise         = Unknown rest
  where
    rest = ys \\ xs

stepGrid :: Grid -> Grid
stepGrid grid = array bounds (map (\(x,e) -> (x,stepCell grid x)) (assocs grid))
  where
    bounds = ((0,0),(8,8))

solve :: Grid -> Grid
solve g 
    | next == g = g
    | otherwise = solve next 
    where
      next = stepGrid g

known :: [Cell] -> [Int]
known = mapMaybe knownValue

solved :: Grid -> Bool
solved g = all isKnown (elems g)
  where
    isKnown (Known _) = True
    isKnown _         = False

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
                   
main = hspec $ do
  describe "Sudoku" $ do
    it "row 0" $ do
      known (row (buildGrid veryEasy) 0) `shouldBe` [6,1,8,5,2]
    it "col 1" $ do
      known (col (buildGrid veryEasy) 1) `shouldBe` [1,5,9,8,4]
    it "subgrid 7,7" $ do
      known (subgrid (buildGrid veryEasy) (7,7)) `shouldBe` [6,4,2,5,8]
    it "eliminates possibilities" $ do
      stepCell (buildGrid veryEasy) (0,8) `shouldBe` (Known 7)
    it "a grid is solved if all of the cells are known" $ do
      solved (buildGrid veryEasy) `shouldBe` False
    it "solves simple examples" $ do
      display (solve (buildGrid veryEasy)) `shouldBe` veryEasySolution
    it "solves an example that requires back-tracking" $ do
      display (solve (buildGrid veryHard)) `shouldBe` veryEasySolution
