module DigitalRoot where

digitalRoot :: Integer -> Integer
digitalRoot n | n < 10 = n
digitalRoot n = digitalRoot (sumDigits 0 n)
  where
    sumDigits :: Integer -> Integer -> Integer
    sumDigits acc n | n < 10 = acc + n
                    | otherwise = sumDigits (acc + (n `mod` 10)) (n `div` 10)

