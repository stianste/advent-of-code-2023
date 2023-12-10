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
    if all (== last values) differences then cumSum else predictNextValue differences (last values + cumSum)


main :: IO ()
main = do
  let line = parseLine "0 3 6 9 12 15"
  print line
  print $ getDifferences [0, 3, 6, 9, 12, 15] == [3, 3, 3, 3, 3]
  print $ predictNextValue [0, 3, 6, 9, 12, 15] 0
  print $ predictNextValue [1, 3, 6, 10, 15, 21] 0
  print $ predictNextValue [10, 13, 16, 21, 30, 45] 0
