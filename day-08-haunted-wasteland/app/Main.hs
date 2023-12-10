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

traverseWithInstructionsMultiple :: ([String], NodeMap, String, Int) -> Int
traverseWithInstructionsMultiple (keys, nodeMap, instructions, depth) = let
    nextDirections = map(\key -> fromJust (Map.lookup key nodeMap)) keys
    nextInstruction = getNextInstruction (instructions, depth)
    nextKeys = map (if nextInstruction == RIGHT then snd else fst ) nextDirections
  in
    if all (elem 'Z') keys then depth else traverseWithInstructionsMultiple(nextKeys, nodeMap, instructions, depth + 1)


main :: IO ()
main = do
  contents <- readFile "./app/inputs/day-08-test-2.txt"
  print $ getNextInstruction ("RLLLR", 0) == RIGHT
  print $ getNextInstruction ("RLLLR", 1) == LEFT
  print $ getNextInstruction ("RLLLR", 4) == RIGHT
  print $ getNextInstruction ("RLLLR", 5) == RIGHT
  print $ getNextInstruction ("RLLLR", 6) == LEFT

  let instructions = "LR"
  -- let instructions = "LLRLRRRLLLRLRRLRRRLRLRRLRLRLRRRLRRRLRLRLRRLLRRRLRRLRRLLRLRRRLRLRLLRRRLLRRRLRLRRRLRRRLRRRLLLRRRLRRLRRLRLRRLRLRRRLRLRRLRLRLRRRLRLLLRRRLLLRLRRRLRLRRLRLRLRLRRLRRLRRLRLRRRLRRRLRRLRRRLRRLRRLRRRLLRLRRLLLRRLRRLRLRLLLRRLRRLRRRLRRLLRLRRRLRRRLRRLRRLRLRRLRLRRRLRRLRRRLLRRRLRLRLLLRRRLLLRRLLRRLRLRRLRLLLRRRR"

  let nodeMap = Map.fromList (map parseLine (lines contents))
  -- let result = traverseWithInstructions ("AAA", nodeMap, instructions, 0)
  let startingNodes = filter (elem 'A') (Map.keys nodeMap)
  print startingNodes

  let result = traverseWithInstructionsMultiple (startingNodes, nodeMap, instructions, 0)

  print result
