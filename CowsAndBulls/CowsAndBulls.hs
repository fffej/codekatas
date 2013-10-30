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

data Player = Player [Row] deriving (Show,Eq)

defaultPlayer :: Player
defaultPlayer = Player allRows

gameInProgress :: GameState -> Bool
gameInProgress (GameState secret p) = score (lastGuess p) secret /= Score 4 0 

data GameState = GameState Row Player deriving (Show,Eq)

lastGuess :: Player -> Row
lastGuess (Player xs) = head xs

remaining :: Player -> [Row]
remaining (Player xs) = xs

makeGuess :: GameState -> GameState
makeGuess (GameState secret player) = GameState secret newPlayer
    where
      guess = lastGuess player
      newPlayer = Player (removeBadGuesses guess (score guess secret) (remaining player))

playGame :: Row -> Int
playGame row = length $ takeWhile gameInProgress games 
    where
      games = iterate makeGuess (GameState row defaultPlayer)
