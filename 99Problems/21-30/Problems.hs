module Problems where

import Test.Hspec
import Test.QuickCheck

import System.Random
import Data.Array.IO hiding (range)
import Control.Monad
import Data.List (sort,tails)

insertAt :: a -> [a] -> Int -> [a]
insertAt e xs 1      = e:xs
insertAt e (x:xs) n  = x : insertAt e xs (n - 1)

-- [s .. e] probably would be cheating!
range :: (Ord a, Enum a) => a -> a -> [a]
range s e = go s
  where
    go n
      | n >= s && n <= e = n : go (succ n)
      | otherwise        = []

rndSelect :: [a] -> Int -> IO [a]
rndSelect xs n = liftM (take n) (shuffle xs)

diffSelect :: Int -> Int -> IO [Int]
diffSelect n r = rndSelect (range 1 r) n

shuffle :: [a] -> IO [a]
shuffle xs = do
  x <- arrayFromList (length xs) xs
  forM_ [n-1,n-2..1] $ \i -> do
    j <- getStdRandom (randomR (0,i))
    tempJ <- readArray x j -- read from j
    tempI <- readArray x i -- read from i
    writeArray x j tempI
    writeArray x i tempJ
  getElems x
  where
    n = length xs
    arrayFromList :: Int -> [a] -> IO (IOArray Int a)
    arrayFromList n = newListArray (0,n-1)

combinations :: Int -> [a] -> [[a]]
combinations 0 _  = [ [] ]
combinations n xs = [ y:ys | y:xs' <- tails xs
                           , ys <- combinations (n-1) xs']

combination :: Int -> [a] -> [([a],[a])]
combination 0 xs     = [([],xs)]
combination n []     = []
combination n (x:xs) = ts ++ ds
  where
    ts = [ (x:ys,zs) | (ys,zs) <- combination (n-1) xs ]
    ds = [ (ys,x:zs) | (ys,zs) <- combination  n    xs ]

group :: [Int] -> [a] -> [[[a]]]
group [] xs = [[]]
group (g:gs) xs = concatMap helper $ combination g xs
  where
    helper (as, bs) = map (as:) (group gs bs)
             
main = hspec $ do
  describe "99 problems" $ do
    it "should insert at" $ do
      insertAt 'X' "abcd" 2 `shouldBe` "aXbcd"
    it "should define range" $ do
      range 4 9 `shouldBe` [4,5,6,7,8,9]
    it "should implement rnd-select" $ do
      rndSelect "abcdefgh" 3 >>= (`shouldSatisfy` (\x -> length x == 3))
    it "diff select" $ do
      diffSelect 6 49 >>= (`shouldSatisfy` (\x -> length x == 6))
    it "random permutation" $ do
      shuffle [1..100] >>= (`shouldSatisfy` (\x -> sort x == [1..100]))
    it "combinations" $ do
      combinations 2 "abc" `shouldBe` ["ab","ac","bc"]
    it "grouping" $ do
      group [2,3,4] ["aldo","beat","carla","david","evi","flip","gary","hugo","ida"] `shouldSatisfy` (\x -> length x == 1260)


