module PIN where

import Data.List

getPINs :: String -> [String]
getPINs = nub . sort . map adjacent

adjacent '1' = "24"
adjacent '2' = "135"
adjacent '3' = "26"
adjacent '4' = "157"
adjacent '5' = "2468"
adjacent '6' = "359"
adjacent '7' = "48"
adjacent '8' = "5780"
adjacent '9' = "68"
adjacent '0' = "8"
