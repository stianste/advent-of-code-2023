module Main where

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Function ((&))
import Data.List (sort, sortBy)

data HandType
    = NoHandType { rank :: Int, hand :: String }
    | Hand { rank :: Int }
    deriving (Eq, Show)

noHandType :: String -> HandType
noHandType = NoHandType 0

pair = Hand 1
twoPairs = Hand 2
threeOfAKind = Hand 3
fullHouse = Hand 4
fourOfAKind = Hand 5
fiveOfAKind = Hand 6

instance Ord HandType where
    compare (NoHandType _ hand1) (NoHandType _ hand2) = compareByStringHand hand1 hand2
    compare (NoHandType _ _) _ = LT
    compare _ (NoHandType _ _) = GT
    compare x y = rank x `compare` rank y

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


getHandType :: String -> HandType
getHandType s
  | isFiveOfAKind s = fiveOfAKind
  | isFourOfAKind s = fourOfAKind
  | isFullHouse s = fullHouse
  | isThreeOfAKind s = threeOfAKind
  | isTwoPairs s = twoPairs
  | isPair s = pair
  | otherwise = noHandType s

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
sortLinesByHandRank xs = sortBy (\a b -> compare (getHandType a) (getHandType b)) (takeWhile (/= " ") xs)


mapIndexed :: (Int -> a -> b) -> [a] -> [b]
mapIndexed f = zipWith f [0..]


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
  print $ fiveOfAKind > fourOfAKind
  print $ pair > noHandType "AABCD"
  print $ noHandType "K5555" > noHandType "Q5555"

  contents <- readFile "./app/inputs/day-07-test.txt"
  let sorted = contents & lines & sortLinesByHandRank
  print sorted
  let rankedBids = map (read . snd . break (==' ')) sorted :: [Int]
  let result = sum (mapIndexed (\i bid -> bid * (i + 1)) rankedBids)
  print result
