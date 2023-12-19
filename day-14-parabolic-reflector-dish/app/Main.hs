module Main where
import Data.List (transpose, partition, intercalate)
import Data.List.Split

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


moveOsLeft :: String -> String
moveOsLeft xs = let (oNum, rest) = partition (=='O') xs
              in oNum ++ rest

moveOs :: String -> String
moveOs = intercalate "#" . map moveOsLeft . splitOn "#"

calculateColumnValue :: String -> Int
calculateColumnValue input = sum $ zipWith (\value index -> if value == 'O' then index else 0) (reverse input) [1..]

parseAndCalculateColumn :: String -> Int
parseAndCalculateColumn input = calculateColumnValue $ moveOs input


main :: IO ()
main = do
  let testColumn = "OOOO..#..."
  let columnResult = parseAndCalculateColumn testColumn
  print columnResult

  content <- readFile "./app/inputs/day-14.txt"
  let columns = transpose $ lines content
  let result = sum $ map parseAndCalculateColumn columns
  print result

