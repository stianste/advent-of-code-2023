module Main where
import qualified Data.Map as Map
import Data.Maybe (fromJust)

data Direction = LEFT | RIGHT deriving (Eq, Show)

type InstructionsMap = Map.Map String (String, String)

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

traverseWithInstructions :: (String, InstructionsMap, String, Int) -> Int
traverseWithInstructions  ("ZZZ", _, _, depth) = depth

traverseWithInstructions (key, instructionsMap, instructions, depth) = let
    nextDirections = fromJust (Map.lookup key instructionsMap)
    nextInstruction = getNextInstruction (instructions, depth)
    nextKey = if nextInstruction == RIGHT then snd nextDirections else fst nextDirections
  in
    traverseWithInstructions (nextKey, instructionsMap, instructions, depth + 1)

main :: IO ()
main = do
  contents <- readFile "./app/inputs/day-08.txt"
  print $ head (map parseLine (lines contents))
  print $ getNextInstruction ("RLLLR", 0) == RIGHT
  print $ getNextInstruction ("RLLLR", 1) == LEFT
  print $ getNextInstruction ("RLLLR", 4) == RIGHT
  print $ getNextInstruction ("RLLLR", 5) == RIGHT
  print $ getNextInstruction ("RLLLR", 6) == LEFT

  -- let instructions = "LLR"
  let instructions = "LLRLRRRLLLRLRRLRRRLRLRRLRLRLRRRLRRRLRLRLRRLLRRRLRRLRRLLRLRRRLRLRLLRRRLLRRRLRLRRRLRRRLRRRLLLRRRLRRLRRLRLRRLRLRRRLRLRRLRLRLRRRLRLLLRRRLLLRLRRRLRLRRLRLRLRLRRLRRLRRLRLRRRLRRRLRRLRRRLRRLRRLRRRLLRLRRLLLRRLRRLRLRLLLRRLRRLRRRLRRLLRLRRRLRRRLRRLRRLRLRRLRLRRRLRRLRRRLLRRRLRLRLLLRRRLLLRRLLRRLRLRRLRLLLRRRR"

  let instructionsMap = Map.fromList (map parseLine (lines contents))
  let result = traverseWithInstructions ("AAA", instructionsMap, instructions, 0)
  print result
