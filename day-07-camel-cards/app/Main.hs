module Main where

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Function ((&))
import Data.List (sort, sortBy)
import Data.Ord (comparing)

data HandType
    = NoHandType { rank :: Int, hand :: String }
    | Pair { rank :: Int, hand :: String}
    | TwoPairs { rank :: Int, hand :: String}
    | ThreeOfAKind { rank :: Int, hand :: String}
    | FullHouse { rank :: Int, hand :: String}
    | FourOfAKind { rank :: Int, hand :: String}
    | FiveOfAKind { rank :: Int, hand :: String}
    deriving (Eq, Show)

noHandType :: String -> HandType
noHandType = NoHandType 0

pair :: String -> HandType
pair = Pair 1

twoPairs :: String -> HandType
twoPairs = TwoPairs 2

threeOfAKind :: String -> HandType
threeOfAKind = ThreeOfAKind 3

fullHouse :: String -> HandType
fullHouse = FullHouse 4

fourOfAKind :: String -> HandType
fourOfAKind = FourOfAKind 5

fiveOfAKind :: String -> HandType
fiveOfAKind = FiveOfAKind 6

instance Ord HandType where
    compare x y = if rank x == rank y then compareByStringHand (hand x) (hand y) else compare (rank x) (rank y)

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
  | isFiveOfAKind s = fiveOfAKind s
  | isFourOfAKind s = fourOfAKind s
  | isFullHouse s = fullHouse s
  | isThreeOfAKind s = threeOfAKind s
  | isTwoPairs s = twoPairs s
  | isPair s = pair s
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

tupleToString :: (String, String) -> String
tupleToString (a, b) = a ++ b

stringList :: [(String, String)] -> [String]
stringList = map tupleToString

sortLinesByHandRank :: [String] -> [String]
sortLinesByHandRank xs = let
    tuples = map (break (==' ')) xs
  in
    stringList (sortBy (comparing (getHandType . fst)) tuples)

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
  print $ fiveOfAKind "" > fourOfAKind ""
  print $ pair "" > noHandType ""
  print $ noHandType "K5555" > noHandType "Q5555"
  print $ fourOfAKind "" > fullHouse ""
  print "A888K"
  let firstHandType = getHandType "A888K"
  print firstHandType
  print "A6AAA"
  let secondHandType = getHandType "A6AAA"
  print secondHandType
  print $ secondHandType > firstHandType

  contents <- readFile "./app/inputs/day-07.txt"
  let sorted = contents & lines & sortLinesByHandRank
  print sorted
  let rankedBids = map (read . snd . break (==' ')) sorted :: [Int]
  let result = sum (mapIndexed (\i bid -> bid * (i + 1)) rankedBids)
  print result
