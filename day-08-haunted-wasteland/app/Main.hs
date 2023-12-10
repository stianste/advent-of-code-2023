module Main where
import qualified Data.Map as Map
import Data.Maybe (fromJust)

data Direction = LEFT | RIGHT deriving (Eq, Show)

type NodeMap = Map.Map String (String, String)

parseLine :: String -> (String, (String, String))
parseLine line = let
    (key, instructions) = break (== '=') line
    parsedInstructions = read (tail instructions) :: (String, String)
  in
    (key, parsedInstructions)

getNextInstruction :: (String, Int) -> Direction
getNextInstruction (instructions, index)
  | instructions !! (index `mod` length instructions) == 'R' = RIGHT
  | otherwise = LEFT

traverseWithInstructions :: (String, NodeMap, String, Int) -> Int
traverseWithInstructions  ("ZZZ", _, _, depth) = depth

traverseWithInstructions (key, nodeMap, instructions, depth) = let
    nextDirections = fromJust (Map.lookup key nodeMap)
    nextInstruction = getNextInstruction (instructions, depth)
    nextKey = if nextInstruction == RIGHT then snd nextDirections else fst nextDirections
  in
    traverseWithInstructions (nextKey, nodeMap, instructions, depth + 1)

traverseWithInstructionsEndsWithZ :: (String, NodeMap, String, Int) -> Int
traverseWithInstructionsEndsWithZ  (key, nodeMap, instructions, depth) = let
    nextDirections = fromJust (Map.lookup key nodeMap)
    nextInstruction = getNextInstruction (instructions, depth)
    nextKey = if nextInstruction == RIGHT then snd nextDirections else fst nextDirections
  in
    if last key == 'Z' then depth else traverseWithInstructionsEndsWithZ (nextKey, nodeMap, instructions, depth + 1)

lcmList :: [Int] -> Int
lcmList = foldl1 lcm

main :: IO ()
main = do
  contents <- readFile "./app/inputs/day-08.txt"
  print $ getNextInstruction ("RLLLR", 0) == RIGHT
  print $ getNextInstruction ("RLLLR", 1) == LEFT
  print $ getNextInstruction ("RLLLR", 4) == RIGHT
  print $ getNextInstruction ("RLLLR", 5) == RIGHT
  print $ getNextInstruction ("RLLLR", 6) == LEFT

  -- let instructions = "LR"
  let instructions = "LLRLRRRLLLRLRRLRRRLRLRRLRLRLRRRLRRRLRLRLRRLLRRRLRRLRRLLRLRRRLRLRLLRRRLLRRRLRLRRRLRRRLRRRLLLRRRLRRLRRLRLRRLRLRRRLRLRRLRLRLRRRLRLLLRRRLLLRLRRRLRLRRLRLRLRLRRLRRLRRLRLRRRLRRRLRRLRRRLRRLRRLRRRLLRLRRLLLRRLRRLRLRLLLRRLRRLRRRLRRLLRLRRRLRRRLRRLRRLRLRRLRLRRRLRRLRRRLLRRRLRLRLLLRRRLLLRRLLRRLRLRRLRLLLRRRR"

  let nodeMap = Map.fromList (map parseLine (lines contents))
  -- let result = traverseWithInstructions ("AAA", nodeMap, instructions, 0)

  let startingNodes = filter (\key -> last key == 'A') (Map.keys nodeMap)
  print startingNodes
  let solutions = map (\key -> traverseWithInstructionsEndsWithZ (key, nodeMap, instructions, 0)) startingNodes
  let solutionSync = lcmList solutions

  print solutionSync
