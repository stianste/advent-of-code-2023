module Main where
import Data.List.Split
import Data.List (intersect)
import Data.Functor ((<&>))
import Data.Function ((&))

calculatePoints :: Int -> Int
calculatePoints 0 = 0
calculatePoints 1 = 1
calculatePoints n = 2 ^ (n - 1)

getPointsFromLine :: String -> Int
getPointsFromLine line = let
    (_, prefixedNumbersString) = break (== ':') line
    (winningNumbersString, numbersString) = break (== '|') (tail prefixedNumbersString)
    winningNumbers = winningNumbersString & words & map read :: [Int]
    numbers = numbersString & tail & words & map read
    intersection = winningNumbers `intersect` numbers
  in
    (calculatePoints . length) intersection


main :: IO ()
main = do
  contents <- readFile "./app/inputs/day-04.txt"
  print $ contents & lines & map getPointsFromLine & sum
