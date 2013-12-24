module Problems where

import Test.Hspec
import Test.QuickCheck

isPrime :: Integer -> Bool
isPrime n = null [ x | x <- [2.. n-1], n `mod` x == 0]

gcd' = undefined

main :: IO ()
main = hspec $ do
  describe "Arithmetic" $ do
    it "can detect primes (positive)" $ do
      isPrime 7
    it "can detect primes (negative)" $ do
      not (isPrime 345)
    it "can do gcd" $ do
      [gcd' 36 63, gcd' (-3) (-6), gcd' (-3) 6] `shouldBe` [9,3,3]
