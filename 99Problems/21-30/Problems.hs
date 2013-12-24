module Problems where

import Test.Hspec
import Test.QuickCheck

insertAt :: a -> [a] -> Int -> [a]
insertAt e xs 1      = e:xs
insertAt e (x:xs) n  = x : insertAt e xs (n - 1)

range :: (Ord a, Enum a) => a -> a -> [a]
range s e = go s
  where
    go n
      | n >= s && n <= e = n : go (succ n)
      | otherwise        = []

rnd_select :: [a] -> Int -> IO [a]
rnd_select xs 0 = return []
rnd_select xs n = undefined

main = hspec $ do
  describe "99 problems" $ do
    it "should insert at" $ do
      insertAt 'X' "abcd" 2 `shouldBe` "aXbcd"
    it "should define range" $ do
      range 4 9 `shouldBe` [4,5,6,7,8,9]
    it "should implement rnd-select" $ do
      rnd_select "abcdefgh" 3 >>= (`shouldSatisfy` (\x -> length x == 3))
