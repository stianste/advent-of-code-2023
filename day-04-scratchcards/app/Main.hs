module Main where

exampleLine :: String
exampleLine = "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53"

main :: IO ()
main = do 
  let splitted = words exampleLine
  print splitted
