module Problems where

import Test.Hspec
import Test.QuickCheck

isPrime :: Integer -> Bool
isPrime n = null [ x | x <- [2.. n-1], n `mod` x == 0]

gcd' :: Integer -> Integer -> Integer
gcd' a b
  | b == 0     = abs a
  | otherwise  = gcd' b (a `rem` b)

coPrime :: Integer -> Integer -> Bool
coPrime x y = gcd x y == 1

totient :: Integer -> Integer
totient n = fromIntegral $ length (filter (coPrime n) [1..n])

main :: IO ()
main = hspec $ do
  describe "Arithmetic" $ do
    it "can detect primes (positive)" $ do
      isPrime 7
    it "can detect primes (negative)" $ do
      not (isPrime 345)
    it "can do gcd" $ do
      [gcd' 36 63, gcd' (-3) (-6), gcd' (-3) 6] `shouldBe` [9,3,3]
    it "co prime (positive)" $ do
      coPrime 35 64
    it "totient function" $ do
      map totient [1,10] `shouldBe` [1,4]
