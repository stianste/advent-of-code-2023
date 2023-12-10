module Main where
import qualified Data.Map as Map

--RL

-- AAA=("BBB", "CCC")
-- BBB=("DDD", "EEE")
-- CCC=("ZZZ", "GGG")
-- DDD=("DDD", "DDD")
-- EEE=("EEE", "EEE")
-- GGG=("GGG", "GGG")
-- ZZZ=("ZZZ", "ZZZ")

type InstructionsMap = Map.Map String (String, String)

parseLine :: String -> (String, (String, String))
parseLine line = let
    (key, instructions) = break (== '=') line
    parsedInstructions = read (tail instructions) :: (String, String)
  in
    (key, parsedInstructions)

-- traverse :: (IntrsuctionsMap, String, Int) -> Int

main :: IO ()
main = do
  contents <- readFile "./app/inputs/day-08-test.txt"
  print $ head (map parseLine (lines contents))
