module Main where
import Data.List.Split
import Data.List (intersect)

exampleLine :: String
exampleLine = "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53"

calculatePoints :: Int -> Int
calculatePoints 0 = 0
calculatePoints 1 = 1
calculatePoints n = 2 ^ (n - 1)

getPointsFromLine :: String -> Int
getPointsFromLine exampleLine = let 
    (gameId, prefixedNumbersString) = break (== ':') exampleLine
    (winningNumbersString, numbersString) = break (== '|') (tail prefixedNumbersString)
    winningNumbers = map read (words winningNumbersString) :: [Int]
    numbers = map read (words (tail numbersString)) :: [Int]
    numOverlapping = length (winningNumbers `intersect` numbers)
  in
    calculatePoints numOverlapping


main :: IO ()
main = do 
  contents <- readFile "./app/inputs/day-04.txt"
  let inputs = lines contents
  let points = map getPointsFromLine inputs
  let result = sum points
  print result
