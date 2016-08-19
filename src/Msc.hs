-- | Maximum surpasser count
module Msc where

import Data.List

-- | Find the maximum surpasser count in O(n*n)
msc0 :: Ord a
     => [a] -> Int
msc0 [] = 0
msc0 xs = maximum [scount z zs|z:zs <- tails xs]

scount :: Ord a
       => a -> [a] -> Int
scount y ys = length (filter (y <) ys)

table0 :: Ord a
       => [a] -> [(a,Int)]
table0 xs = [(z,scount z zs)|z:zs <- tails xs]

-- | Find the maximum surpasser count in O(nlogn)
msc :: Ord a
    => [a] -> Int
msc [] = 0
msc xs = (maximum . map snd . table) xs

table :: Ord t
      => [t] -> [(t,Int)]
table [x] = [(x,0)]
table xs =
  tjoin (m - n)
        (table ys)
        (table zs)
  where m = length xs
        n = m `div` 2
        (ys,zs) = splitAt n xs

tjoin :: (Eq t,Num t,Ord a)
      => t -> [(a,t)] -> [(a,t)] -> [(a,t)]
tjoin 0 txs [] = txs
tjoin _ [] tys = tys
tjoin n txs@((x,c):txs') tys@((y,d):tys')
  | x < y = (x,c + n) : tjoin n txs' tys
  | x >= y = (y,d) : tjoin (n - 1) txs tys'