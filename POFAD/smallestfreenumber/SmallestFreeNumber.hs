module SmallestFreeNumber where 

import Data.List ((\\))

minFree :: [Integer] -> Integer
minFree xs = head ([0 .. ] \\ xs)