module Main where
import Data.List (transpose)

exampleInput :: String
exampleInput = "O....#...." ++
               "O.OO#....#" ++
               ".....##..." ++
               "OO.#O....O" ++
               ".O.....O#." ++
               "O.#..O.#.#" ++
               "..O..#O..O" ++
               ".......O.." ++
               "#....###.." ++
               "#OO..#...."

partialTriangleFunction :: Int -> Int -> Int
partialTriangleFunction n numberToTake = sum $ take numberToTake [n,(n-1)..]

calculateColumnValue :: String -> Int -> Int
calculateColumnValue input startValue = let
    numToTake = length $ filter (/='.') (takeWhile (/='#') input)
  in
    partialTriangleFunction startValue numToTake


main :: IO ()
main = do
  let test = partialTriangleFunction 10 4
  print test
  let testColumn = "0000..#"
  let columnResult = calculateColumnValue testColumn 10

  content <- readFile "./app/inputs/day-14-test.txt"
  let columns = transpose $ lines content
  let m = length $ head columns
  print m
  print columns
  let results = map (`calculateColumnValue` m) columns
  print results
  let result = sum results
  print result

