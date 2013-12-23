module Problems where

import Test.Hspec
import Test.QuickCheck

import Control.Monad
import Control.Arrow ((&&&))
import Control.Applicative

myLast :: [a] -> a
myLast (x:[]) = x
myLast (_:xs) = myLast xs

myButLast :: [a] -> a
myButLast (x:y:[]) = x
myButLast (x:xs)   = myButLast xs

elementAt :: [a] -> Int -> a
elementAt (x:_ ) 1 = x
elementAt (_:xs) n = elementAt xs (n - 1)

myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs 

myReverse :: [a] -> [a]
myReverse xs = go xs []
  where
    go xs ys
      | null xs   = ys
      | otherwise = go (tail xs) (head xs:ys)  

isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == myReverse xs

data NestedList a = Elem a
                  | List [NestedList a]
                    deriving (Show,Eq)

flatten :: NestedList a -> [a]
flatten (Elem a)      = [a]
flatten (List [])     = []
flatten (List (x:xs)) = flatten x ++ flatten (List xs)

compress :: Eq a => [a] -> [a]
compress []     = []
compress (x:xs) = x : go xs x
  where
    go [] ls = []
    go (x:xs) y
      | x == y    = go xs y
      | otherwise = x : go xs x

pack :: Eq a => [a] -> [[a]]
pack []     = []
pack (x:xs) = take' x xs : pack (drop' x xs)
  where
    take' y [] = [y]
    take' y (z:zs)
      | y == z    = z : take' y zs
      | otherwise = [y]
    drop' y [] = []
    drop' y (z:zs)
      | y /= z    = z:zs
      | otherwise = drop' y zs 

-- Open question; why does this parse in ghci and not hlint?
-- encode :: Eq a => [a] => [(Int,a)]
encode :: Eq a => [a] -> [(Int,a)]
encode xs = map (length &&& head) (pack xs)

data CompressToken a = Multiple Int a
                     | Single a
                       deriving (Eq,Show)

encodeModified :: Eq a => [a] -> [CompressToken a]
encodeModified xs = map toToken (pack xs)
  where
    toToken (x:[]) = Single x
    toToken xs = Multiple (length xs) (head xs)

encodeDirect :: Eq a => [a] -> [CompressToken a]
encodeDirect []     = []
encodeDirect (x:xs) = makeToken count x : encodeDirect rest
  where
    makeToken 1 c = Single c
    makeToken n c = Multiple n c
    (count,rest) = go x xs 1
    go x []     acc = (acc,[])
    go x (y:ys) acc 
      | x /= y = (acc,y:ys)
      | x == y = go x ys (acc + 1)

decodeModified :: Eq a => [CompressToken a] -> [a]
decodeModified = concatMap fromToken 
  where
    fromToken (Single x) = [x]
    fromToken (Multiple n x) = replicate n x

duplicate :: [a] => [a]
duplicate [] = []
duplicate (x:xs) = x : x : duplicate xs

repli = undefined

main :: IO ()
main = hspec $ do
  describe "List functions" $ do
    it "should find the last element of the list" $ do
      myLast [1,2,3,4] `shouldBe` 4
    it "last of an empty list should be an error" $ do
      myLast [] `shouldThrow` anyException
    it "should find last but one element of the list" $ do
      myButLast [1,2,3,4] `shouldBe` 3
    it "element at finds the nth element of the list" $ do
      elementAt [1,2,3] 2 `shouldBe` 2
    it "finds the length of a list" $ do
      myLength "Hello, world!" `shouldBe` 13
    it "myReverse reverses a string" $ do
      myReverse [1,2,3,4] `shouldBe` [4,3,2,1]
    it "checks for palindomes (positive)" $ do
      isPalindrome [1,2,3,2,1]
    it "checks for palindrome (negative)" $ do
      not (isPalindrome [1,2,3])
    it "flattens nested lists" $ do
      flatten (Elem 5) `shouldBe` [5]
    it "really does flatten lists" $ do
      flatten (List [List [Elem 5]]) `shouldBe` [5]
    it "eliminates consecutive duplicates" $ do
      compress "aaabbbcdddeee" `shouldBe` "abcde"
    it "packs consecutive duplicates into sublists" $ do
      pack "aaaabbbccd" `shouldBe` ["aaaa","bbb","cc","d"]
    it "performs run length encoding" $ do
      encode "aaaabbbccd" `shouldBe` [(4,'a'),(3,'b'),(2,'c'),(1,'d')]
    it "eliminates redundancy during compress" $ do
      encodeModified "aaaabbbccd" `shouldBe` [Multiple 4 'a'
                                             ,Multiple 3 'b'
                                             ,Multiple 2 'c'
                                             ,Single     'd']
    it "decode modified" $ do
      decodeModified [Multiple 4 'a'
                     ,Multiple 3 'b'
                     ,Multiple 2 'c'
                     ,Single     'd'] `shouldBe` "aaaabbbccd"
    it "decode modified and encode modified work the same" $ property $
      \xs -> decodeModified (encodeModified xs) == (xs :: [Int])
    it "eliminates redundancy during compress (efficient)" $ do
      encodeDirect "aaaabbbccd" `shouldBe` [Multiple 4 'a'
                                           ,Multiple 3 'b'
                                           ,Multiple 2 'c'
                                           ,Single     'd']
    it "duplicate the items in a list" $ do
      duplicate [1,2,3] `shouldBe` [1,1,2,2,3,3]
    it "duplicate doubles the length" $ property $
      \xs -> length (duplicate xs) == 2 * length (xs :: [Int])
    it "replicate elements" $ do
      repli [1,2,3] 3 `shouldBe` [1,1,1,2,2,2,3,3,3]
