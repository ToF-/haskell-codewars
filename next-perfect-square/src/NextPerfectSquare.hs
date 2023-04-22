module NextPerfectSquare where

findNextSquare :: Integer -> Integer
findNextSquare n | isPerfectSquare n = nextSquare n
                 | otherwise = -1

isPerfectSquare :: Integer -> Bool
isPerfectSquare n = fromIntegral (truncate rt) == rt
    where
        rt :: Double
        rt = sqrt (fromInteger n)

nextSquare :: Integer -> Integer
nextSquare n = rt * rt
    where
        rt = 1 + round (sqrt (fromInteger n))
