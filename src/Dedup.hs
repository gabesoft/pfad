-- | Removing duplicates from a list
module Dedup where

import qualified Data.Set as Set

dedup0 :: Eq a
       => [a] -> [a]
dedup0 [] = []
dedup0 (x:xs) = x : dedup0 (xs \\ x)

dedup :: (Ord a,Eq a)
      => [a] -> [a]
dedup = go Set.empty
  where go _ [] = []
        go seen (y:ys)
          | Set.member y seen = go seen ys
          | otherwise = y : go (Set.insert y seen) ys

(\\) :: Eq a
     => [a] -> a -> [a]
(\\) [] _ = []
(\\) (y:ys) x
  | x == y = ys \\ x
  | otherwise = y : (ys \\ x)