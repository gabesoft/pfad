-- | Finding all common prefixes of a list
module Allcp where

import Data.List (tails)

-- | Compute the longest common sequence
llcp :: (Eq a1,Num a)
     => [a1] -> [a1] -> a
llcp xs [] = 0
llcp [] ys = 0
llcp (x:xs) (y:ys)
  | x == y = 1 + llcp xs ys
  | otherwise = 0

-- | Compute all longest common sequences
allcp :: (Eq a,Num b)
      => [a] -> [b]
allcp xs = llcp xs <$> netails xs

-- | Same as the built-in tails but without an empty element at the end
netails :: [a] -> [[a]]
netails = init . tails