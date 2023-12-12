module Main where

-- 0 3 6 9 12 15 – 18
-- 1 3 6 10 15 21 – 28
-- 10 13 16 21 30 45 – 68

getDifferences :: [Int] -> [Int]
getDifferences xs = zipWith (-) (tail xs) xs

parseLine :: String -> [Int]
parseLine = map read . words

predictNextValue :: [Int] -> Int -> Int
predictNextValue values cumSum = let
    differences = getDifferences values
  in
    if all (== 0) differences then last values + cumSum else predictNextValue differences (last values + cumSum)


main :: IO ()
main = do
  let line = parseLine "0 3 6 9 12 15"
  print line
  print $ getDifferences [0, 3, 6, 9, 12, 15] == [3, 3, 3, 3, 3]
  print $ predictNextValue [0, 3, 6, 9, 12, 15] 0 == 18
  print $ predictNextValue [1, 3, 6, 10, 15, 21] 0 == 28
  print $ predictNextValue [10, 13, 16, 21, 30, 45] 0 == 68
  content <- readFile "./app/inputs/day-09.txt"
  let result = map (\line -> predictNextValue (parseLine line) 0) (lines content)
  print result
  print $ sum result
