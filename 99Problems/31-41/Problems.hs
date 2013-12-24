module Problems where

import Test.Hspec
import Test.QuickCheck

isPrime :: Integer -> Bool
isPrime n = null [ x | x <- [2.. n-1], n `mod` x == 0]

main :: IO ()
main = hspec $ do
  describe "Arithmetic" $ do
    it "can detect primes (positive)" $ do
      isPrime 7
    it "can detect primes (negative)" $ do
      not (isPrime 345)
