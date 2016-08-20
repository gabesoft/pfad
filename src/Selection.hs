-- | Selecting the kth smallest element from two disjoint sets
module Selection where

import qualified Data.List as L

smallest0 :: Ord a
          => Int -> ([a],[a]) -> a
smallest0 k (xs,ys) = sunion (xs,ys) !! k

-- | Merge two disjoint lists each in increasing order
sunion :: Ord a
       => ([a],[a]) -> [a]
sunion (xs,[]) = xs
sunion ([],ys) = ys
sunion (x:xs,y:ys)
  | x < y = x : sunion (xs,y : ys)
  | x > y = y : sunion (x : xs,ys)

smallest :: Ord a => Int -> ([a], [a]) -> a
smallest k ([],ws) = ws !! k
smallest k (zs,[]) = zs !! k
smallest k (zs,ws) =
  case (a < b,k <= p + q) of
    (True,True) -> smallest k (zs,us)
    (True,False) ->
      smallest (k - p - 1)
               (ys,ws)
    (False,True) -> smallest k (xs,ws)
    (False,False) ->
      smallest (k - q - 1)
               (zs,vs)
  where p = length zs `div` 2
        q = length ws `div` 2
        (xs,a:ys) = L.splitAt p zs
        (us,b:vs) = L.splitAt q ws