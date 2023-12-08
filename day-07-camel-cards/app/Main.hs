module Main where

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Function ((&))
import Data.List (sort, sortBy)

data HandType = Pair | TwoPairs | IsThreeOfAKind | FullHouse | FourOfAKind | FiveOfAKind deriving (Eq, Ord)

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


--example of one line: "32T3K 765", which should be one pair
getHandType :: String -> HandType
getHandType s
  | isFiveOfAKind s = FiveOfAKind
  | isFourOfAKind s = FourOfAKind
  | isFullHouse s = FullHouse
  | isThreeOfAKind s = IsThreeOfAKind
  | isTwoPairs s = TwoPairs
  | isPair s = Pair
  | otherwise = error ("No hand type found for input: " ++ s)

sortHand :: HandType -> HandType -> Ordering
sortHand firstHand secondHand
  | firstHand == secondHand = EQ
  | otherwise = compare firstHand secondHand

--KK677 and KTJJT are both two pair
compareByCardValues :: Char -> Char -> Ordering
compareByCardValues firstCard secondCard
  | firstCard == secondCard = EQ
  | firstCard == 'A' = GT
  | secondCard == 'A' = LT
  | firstCard == 'K' = GT
  | secondCard == 'K' = LT
  | firstCard == 'Q' = GT
  | secondCard == 'Q' = LT
  | firstCard == 'J' = GT
  | secondCard == 'J' = LT
  | firstCard == 'T' = GT
  | secondCard == 'T' = LT
  | otherwise = compare firstCard secondCard

compareByStringHand :: String -> String -> Ordering
compareByStringHand firstHand secondHand
  | head firstHand == head secondHand = compareByStringHand (tail firstHand) (tail secondHand)
  | otherwise = compareByCardValues (head firstHand) (head secondHand)

sortLinesByHandRank :: [String] -> [String]
sortLinesByHandRank xs = let
    preSortedHands = sortBy compareByStringHand xs & break (==" ") & fst
  in
    preSortedHands --sortBy sortHand (map getHandType hands)


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
  print $ FiveOfAKind > FourOfAKind
  contents <- readFile "./app/inputs/day-07-test.txt"
  let hands = ["KK677", "KTJJT" , "Q1111", "T1111", "J5555", "A1111"] --contents & lines
  print $ hands & sortBy compareByStringHand
