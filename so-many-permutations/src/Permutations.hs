module Permutations where

import qualified Data.List as L (concatMap, nub, sort, splitAt)
permutations :: String -> [String]
permutations = L.nub . L.sort . p

p :: String -> [String]
p [] = [[]]
p [c] = [[c]]
p (c:cs) = L.concatMap (ms c) (p cs)

ms :: Char -> String -> [String]
ms c s = map (merge c) (splits s)

splits :: String -> [(String, String)]
splits s = map (\n -> L.splitAt n s) [0..length s]

merge :: Char -> (String, String) -> String
merge c (p,s) = p ++ [c] ++ s

-- p abcd
-- ms a (p bcd)
-- concat (ms a) [bcd,bdc,cbd,cdb,dcb,dbc]
-- [abcd,bacd,bcad,bcda,abdc, 
-- abc bac bca acb 
--
-- ["" "bc"] ["b" "c"] ["bc" ""]
--
