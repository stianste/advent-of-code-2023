main :: IO ()
main = do
  contents <- readFile "../../inputs/day-01.txt"
  putStrLn contents
