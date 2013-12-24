module Problems where

import Data.List (group)
import Control.Arrow ((&&&))

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

primeFactors :: Integral a => a -> [a]
primeFactors 0 = []
primeFactors n
  | not (null divisors) = divisor : primeFactors (n `div` divisor)
  | otherwise           = []
  where
    divisors = filter (\x -> n `mod` x == 0) [2..n]
    divisor  = head divisors

primeFactorsMult :: Integral a => a -> [(a,Int)]
primeFactorsMult n = map (head &&& length) $ group (primeFactors n)

effTotient :: Integer -> Integer
effTotient n = product mp
  where
    pf = primeFactorsMult n
    mp = map (\(p,m) -> p - 1 * p ^ (m - 1)) pf

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
    it "prime factors" $ do
      primeFactors 315 `shouldBe` [3,3,5,7]
    it "prime factors and multiplicity" $ do
      primeFactorsMult 315 `shouldBe` [(3,2),(5,1),(7,1)]
    it "efficient totient function" $ do
      map effTotient [1,10] `shouldBe` [1,4]
