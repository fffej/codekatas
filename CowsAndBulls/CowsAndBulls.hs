module CowsAndBulls where

data Row = Row Int Int Int Int deriving (Show,Eq)

data Score = Score Int Int deriving (Show,Eq)

rowToList :: Row -> [Int]
rowToList (Row a b c d) = [a,b,c,d]

score :: Row -> Row -> Score
score a b = Score bulls cows
    where
      bulls = calculateBulls a b 
      cows = calculateCows a b

calculateCows :: Row -> Row -> Int
calculateCows a b = length $ filter (\(x,y) -> y `elem` xs) nonBulls
    where
      xs = rowToList a
      ys = rowToList b
      nonBulls = filter (uncurry (/=)) (zip xs ys)

calculateBulls :: Row -> Row -> Int
calculateBulls a b = length $ filter id (zipWith (==) xs ys)
    where
      xs = rowToList a
      ys = rowToList b

allRows :: [Row]
allRows = [Row a b c d | a <- [0..9], b <- [0..9], c <- [0..9], d <- [0..9], distinct a b c d]
    where
      distinct a b c d = a /= b && a /= c && a /= d &&
                         b /= c && b /= d &&
                         c /= d
                           
removeBadGuesses :: Row -> Score -> [Row] -> [Row]
removeBadGuesses r s = filter (\row -> score row r == s)
