{-
Consider the problem of computing the smallest natural number not
in a given finite set X of natural numbers.
-}

module SmallestFreeNumber where 

import Data.List ((\\))

-- Naive definition.  This is O(N^2)
minFree :: [Integer] -> Integer
minFree xs = head ([0 .. ] \\ xs)