module Main where
import Data.List.Split
import Data.List (intersect)

exampleLine :: String
exampleLine = "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53"

calculatePoints :: Int -> Int
calculatePoints 0 = 0
calculatePoints 1 = 1
calculatePoints n = n * 2


main :: IO ()
main = do 
  print exampleLine

  let (gameId, prefixedNumbersString) = break (== ':') exampleLine
  print (gameId, tail prefixedNumbersString)

  let (winningNumbersString, numbersString) = break (== '|') (tail prefixedNumbersString)
  print (winningNumbersString, numbersString)

  let winningNumbers = map read (words winningNumbersString) :: [Int]
  let numbers = map read (words (tail numbersString)) :: [Int]
  print (winningNumbers, numbers)

  let numOverlapping = length (winningNumbers `intersect` numbers)
  print numOverlapping
