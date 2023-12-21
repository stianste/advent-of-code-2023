module Main where
import Data.Function ((&))
import Data.List.Split (splitOn)
import Data.Char (ord)

hashAlgorithm :: Int -> Int -> [Char] -> Int
hashAlgorithm currentSum index target
  | index == length target = currentSum
  | otherwise = hashAlgorithm (target !! index & ord & (+) currentSum & (*) 17 & (`mod` 256)) (index + 1) target

main :: IO ()
main = do
  print $ hashAlgorithm 0 0 "HASH"
  content <- readFile "./app/inputs/day-15.txt"
  let result = sum $ map (hashAlgorithm 0 0) (splitOn "," content)
  print result
