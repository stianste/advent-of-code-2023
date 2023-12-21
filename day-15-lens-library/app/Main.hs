module Main where
import Data.Function ((&))


import Data.Char

hashAlgorithm :: Int -> Int -> [Char] -> Int
hashAlgorithm currentSum index target
  | index == length target = currentSum
  | otherwise = hashAlgorithm (target !! index & ord & (+) currentSum & (*) 17 & (`mod` 256)) (index + 1) target

main :: IO ()
main = do
  print $ hashAlgorithm 0 0 "HASH"
