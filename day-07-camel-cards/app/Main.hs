module Main where

import qualified Data.Set as Set
import qualified Data.Map as Map

countOccurrences :: String -> Map.Map Char Int
countOccurrences str = Map.fromListWith (+) [(c, 1) | c <- str]

countOccurrencesOfInts :: [Int] -> Map.Map Int Int
countOccurrencesOfInts listOfInts = Map.fromListWith (+) [(c, 1) | c <- listOfInts]

isFiveOfAKind :: String -> Bool
isFiveOfAKind s = Set.size (Set.fromList s) == 1

isFourOfAKind :: String -> Bool
isFourOfAKind s = let
    counts = Map.elems (countOccurrences s)
  in
    4 `elem` counts

isFullHouse :: String -> Bool
isFullHouse  s = let
    counts = Map.elems (countOccurrences s)
  in
    3 `elem` counts && 2 `elem` counts

isThreeOfAKind :: String -> Bool
isThreeOfAKind s = let
    counts = Map.elems (countOccurrences s)
  in
    3 `elem` counts && 2 `notElem` counts

isTwoPairs :: String -> Bool
isTwoPairs s = let
    counts = Map.elems (countOccurrences s)
  in
    Map.lookup 2 (countOccurrencesOfInts counts) == Just 2

isPair :: String -> Bool
isPair s = let
    counts = Map.elems (countOccurrences s)
  in
    2 `elem` counts && not (isFullHouse s) && not (isTwoPairs s)


main :: IO ()
main = do
  print $ isFiveOfAKind "55555"
  print $ isFourOfAKind "A5555"
  print $ isFullHouse "AA555"
  print $ not (isThreeOfAKind "AA555") --because full house
  print $ isThreeOfAKind "AB555"
  print $ isTwoPairs "AA5BB"
  print $ not (isTwoPairs "AA555")
  print $ not (isPair "AA5BB")
  print $ isPair "AABCD"
