module CowsAndBulls where

import Data.List

data Row = Row Int Int Int Int deriving (Show,Eq)
data Score = Score Int Int deriving (Show,Eq)
data Player = Player [Row] deriving (Show,Eq)

rowToList :: Row -> [Int]
rowToList (Row a b c d) = [a,b,c,d]

score :: Row -> Row -> Score
score a b = Score bulls cows
    where
      xs = rowToList a 
      ys = rowToList b
      nonBulls = filter (uncurry (/=)) (zip xs ys)
      cows = length $ filter (\(x,y) -> y `elem` xs) nonBulls
      bulls = length $ filter id (zipWith (==) xs ys)

allRows :: [Row]
allRows = filter distinct [Row a b c d | a <- [0..9], b <- [0..9], c <- [0..9], d <- [0..9]]
    where 
      distinct (Row a b c d) = 4 == length (nub [a,b,c,d])
                           
removeBadGuesses :: Row -> Score -> [Row] -> [Row]
removeBadGuesses r s = filter (\row -> score row r == s)

lastGuess :: Player -> Row
lastGuess (Player xs) = head xs

playGame :: Row -> Int
playGame secret = length $ takeWhile gameInProgress games 
    where
      games = iterate makeGuess (Player allRows)
      gameInProgress (Player (x:xs)) = score x secret /= Score 4 0
      makeGuess (Player guesses) = newPlayer
          where
            guess = head guesses
            newPlayer = Player (removeBadGuesses guess (score guess secret) guesses)

