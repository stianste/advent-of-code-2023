import Data.Char ( isDigit, digitToInt )

removeNonDigit :: String -> String
removeNonDigit = filter isDigit

getFirstDigit :: String -> Int
getFirstDigit x = head (map digitToInt x)

getLastDigit :: String -> Int
getLastDigit x = last (map digitToInt x)

extractNumberFromString :: String -> String
extractNumberFromString s = show (getFirstDigit s) ++ show (getLastDigit s)

toInt :: String -> Int
toInt it = read it :: Int

main :: IO ()

main = do
  contents <- readFile "../../inputs/day-01.txt"
  let inputs = lines contents
  let inputsAsDigits = map removeNonDigit inputs
  let firstAndLastDigits = map extractNumberFromString inputsAsDigits
  let result = sum (map toInt firstAndLastDigits)
  print result

  -- 54304 Was the correct answer
