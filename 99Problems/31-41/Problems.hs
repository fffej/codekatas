module Problems where

import Test.Hspec
import Test.QuickCheck

isPrime :: Integer -> Bool
isPrime = undefined

main :: IO ()
main = hspec $ do
  describe "Arithmetic" $ do
    it "can detect primes (positive)" $ do
      isPrime 7
    it "can detect primes (negative)" $ do
      not (isPrime 345)
