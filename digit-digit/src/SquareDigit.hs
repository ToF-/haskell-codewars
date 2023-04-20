module SquareDigit where
import Data.Char

squareDigit :: Int -> Int
squareDigit 0 = 0
squareDigit n | n < 0 = negate (squareDigit (negate n))
squareDigit n = read (foldl squareDigit' "" (digits n))
  where
    squareDigit' acc d = show (d * d) ++ acc
    digits n = digits' n
    digits' 0 = []
    digits' n = n `mod` 10 : digits' (n `div` 10)

