module NextPerfectSquare where

findNextSquare :: Integer -> Integer
findNextSquare n = 
  let rt = sqrt n
      ps = (round rt) == truncate rt
   in if ps then (rt + 1) * (rt + 1) else -1
