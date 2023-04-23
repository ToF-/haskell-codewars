module BWT where

import Data.List

-- | Encode an input sequence with the Burrows-Wheeler-Transformation.
encode :: Ord a => [a] -> ([a], Int)
encode [] = ([], 0)
encode s = (key, i)
    where 
        key = last (transpose rots)
        i = index 0 s rots
        rots = sort (rotations s)

        index n t [] = error "cannot find back elem"
        index n t (s:ss) | s == t = n
                         | otherwise = index (succ n) t ss

        rotations s = take (length s) (iterate rotation s)
        rotation s = head rs : reverse (tail rs)
            where rs = reverse s

-- | Get back the input from a Burrows-Wheeler-Transformation.
decode :: Ord a => [a] -> Int -> [a]
decode [] _ = []
decode s i = (iterate process initial) !! n !! i
    where
        process = sort . (insert1stCol firstCol)
        firstCol = [[c] | c <- s]
        n = length s
        initial = take n (repeat [])
        insert1stCol = zipWith (++)

