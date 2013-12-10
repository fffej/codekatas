{-
Consider the problem of computing the smallest natural number not
in a given finite set X of natural numbers.
-}

module SmallestFreeNumber where 

import Data.List ((\\))
import Data.Array

-- Naive definition.  This is O(N^2)
minFree :: [Integer] -> Integer
minFree xs = head ([0 .. ] \\ xs)

-- Key fact: Not every number in the range [0 .. length xs] 
-- can be in xs
search :: Array Int Bool -> Int
search = length . takeWhile id . elems

-- accumArray is mental
-- arg one transforms array entries and values into new entries
-- arg two is initial index
-- arg three defines lower and upper bounds
-- arg fourth is an association list

checklist :: [Int] -> Array Int Bool
checklist xs = accumArray (||) False (0,n) (zip numsLessThanN (repeat True))
    where
        n = length xs
        numsLessThanN = filter (<= n) xs

minFree2 :: [Integer] -> Integer
minFree2 = search . checklist	       