module Main where

import qualified Data.Map as Map
import Data.List.Split

type ColorMap = Map.Map String Int
colorMap :: ColorMap
colorMap = Map.fromList [("red", 12), ("green", 13), ("blue", 14)]

replaceSemicolon :: String -> String
replaceSemicolon = map (\c -> if c == ';' then ',' else c)

-- Example: "Game 2:1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue" -> "1 blue, 2 green, 3 green, 4 blue, 1 red, 1 green, 1 blue"

getRevealTuples :: String -> [(String, Int)]
getRevealTuples s = let
    (gameNumber, reveals) = break (== ':') s
    sanitisedReveals = replaceSemicolon (tail reveals)
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


main :: IO ()
main = do
  let tuples = getRevealTuples "Game 1:3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
  print tuples
  print $ anyViolations colorMap tuples
