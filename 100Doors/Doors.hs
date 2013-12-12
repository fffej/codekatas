module Doors where

data Door = Door Int Bool deriving Show

-- 100 doors, initially closed.
mkDoors :: [Door]
mkDoors = [Door x True | x <- [1..100]]

-- Visit the nth room, changing state
visitNth :: Int -> [Door] -> [Door]
visitNth n = map (flickDoor n)

-- flick the door based on the step
flickDoor :: Int -> Door -> Door
flickDoor n (Door x state)
    | x `mod` n == 0 = Door x (not state)
    | otherwise = Door x state

openDoorsAfter100 :: [Door]
openDoorsAfter100  = filter open (foldl (flip visitNth) mkDoors [1..100])
    where
      open (Door _ True) = False
      open (Door _ False) = True
