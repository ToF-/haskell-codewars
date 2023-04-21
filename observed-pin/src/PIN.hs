module PIN where

import Data.List

getPINs :: String -> [String]
getPINs = nub . sort . foldl combineAdjacent [""]
  where
    combineAdjacent acc c = combine acc (withAdjacent c)
    combine xxs ys = [ xs ++ [y] | xs <- xxs, y <- ys]
    withAdjacent c = c : adjacent c

    adjacent '1' = "24"
    adjacent '2' = "135"
    adjacent '3' = "26"
    adjacent '4' = "157"
    adjacent '5' = "2468"
    adjacent '6' = "359"
    adjacent '7' = "48"
    adjacent '8' = "57890"
    adjacent '9' = "68"
    adjacent '0' = "8"
