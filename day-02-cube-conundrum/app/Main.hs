module Main where

import qualified Data.Map as Map
import Data.List.Split

type ColorMap = Map.Map String Int
colorMap :: ColorMap
colorMap = Map.fromList [("red", 12), ("green", 13), ("blue", 14)]

replaceSemicolon :: String -> String
replaceSemicolon = map (\c -> if c == ';' then ',' else c)

getGameId :: String -> Int
getGameId s = let
    (gameId, _) = break (== ':') s
  in
    read (last (words gameId)) :: Int

getSanitisedReveals :: String -> String
getSanitisedReveals s = let
    (_, reveals) = break (== ':') s
  in
    replaceSemicolon (tail reveals)

getRevealTuples :: String -> [(String, Int)]
getRevealTuples s = let
    sanitisedReveals = getSanitisedReveals s
    revealList = splitOn ", " sanitisedReveals
    tupleList = map (break (== ' ')) revealList
  in
    [(tail (snd x), read (fst x) :: Int) | x <- tupleList ]

tupleIsInvalid  :: ColorMap -> (String, Int) -> Bool
tupleIsInvalid colorMap (color, presentedValue) = case Map.lookup color colorMap of
  Just maxValue ->  presentedValue > maxValue
  Nothing -> False

anyViolations :: ColorMap -> [(String, Int)] -> Bool
anyViolations colorMap = any (tupleIsInvalid colorMap)

getGameIdIfNotViolatedElseZero :: ColorMap -> String -> Int 
getGameIdIfNotViolatedElseZero colorMap s = let
    tuples = getRevealTuples s
  in
    if not (anyViolations colorMap tuples) then getGameId s else 0

main :: IO ()
main = do
  let exampleString = "Game 1:3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
  let tuples = getRevealTuples exampleString 
  print tuples
  print $ anyViolations colorMap tuples
  print $ getGameId exampleString 
  contents <- readFile "./app/inputs/day-02.txt"
  let violatingIds = map (getGameIdIfNotViolatedElseZero colorMap) (lines contents)
  let result = sum violatingIds
  print violatingIds
  print result
