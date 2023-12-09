
module Utils where

import qualified Data.Map as Map

countOccurrences :: (Ord a) => [a] -> Map.Map a Int
countOccurrences xs = Map.fromListWith (+) [(x, 1) | x <- xs]

joinTupleToString :: (String, String) -> String
joinTupleToString (a, b) = a ++ b

mapIndexed :: (Int -> a -> b) -> [a] -> [b]
mapIndexed f = zipWith f [0..]
