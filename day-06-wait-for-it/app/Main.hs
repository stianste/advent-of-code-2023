module Main where

calculateLength :: Int -> Int -> Int
calculateLength raceDuration candidateHoldTime = candidateHoldTime * (raceDuration - candidateHoldTime)

processRace :: (Int, Int) -> Int
processRace race = let
    (duration, record) = race
    candidates = [1..(fst race - 1)]
  in
    length $ filter (> record) (map (calculateLength duration) candidates)

main :: IO ()
main = do
  -- let races = [(7, 9), (15, 40), (30, 200)]
  let races = [(59, 543), (68, 1020), (82, 1664), (74, 1022)] 
  -- let part1 = product $ map processRace races
  -- print part1

  let part2 = processRace (59688274, 543102016641022)
  print part2
