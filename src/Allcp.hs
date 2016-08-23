-- | Finding all common prefixes of a list
module Allcp (allcp0, allcp) where

import Data.List (tails)
import qualified Data.Vector as V

-- | Compute the longest common sequence
llcp :: (Eq a1,Num a)
     => [a1] -> [a1] -> a
llcp xs [] = 0
llcp [] ys = 0
llcp (x:xs) (y:ys)
  | x == y = 1 + llcp xs ys
  | otherwise = 0

-- | Compute all longest common sequences
allcp0 :: (Eq a,Num b)
       => [a] -> [b]
allcp0 xs = llcp xs <$> netails xs

-- | Same as the built-in tails but without an empty element at the end
netails :: [a] -> [[a]]
netails = init . tails

step1
  :: Eq a1
  => [a1] -> ([Int],Int,Int,Int) -> ([Int],Int,Int,Int)
step1 xs (as,i,p,k)
  | k >= i + p = (snoc as a,k,a,k + 1)
  | q /= r = (snoc as (min q r),i,p,k + 1)
  | q == r = (snoc as b,k,b,k + 1)
  where q = as !! (k - i)
        r = p - (k - i)
        a = llcp xs (drop k xs)
        b =
          q +
          llcp (drop q xs)
               (drop (q + k) xs)
        snoc ys y = ys ++ [y]

allcp1 :: Eq a
       => [a] -> [Int]
allcp1 [] = []
allcp1 xs =
  fst4 $
  until (done n)
        (step1 xs)
        ([n],0,0,1)
  where n = length xs

done :: (Ord b, Eq b)
     => b -> (t,t1,t2,b) -> Bool
done k = (>= k) . lst4

lst4 :: (t,t1,t2,t3) -> t3
lst4 (_,_,_,d) = d

fst4 :: (t,t1,t2,t3) -> t
fst4 (a,_,_,_) = a

allcp :: Eq a
      => [a] -> [Int]
allcp [] = []
allcp xs =
  (reverse . fst4) $
  until (done n)
        (step xa)
        ([n],0,0,1)
  where n = length xs
        xa = V.fromList xs

step
  :: Eq a
  => V.Vector a -> ([Int],Int,Int,Int) -> ([Int],Int,Int,Int)
step xa (as,i,p,k)
  | k >= i + p = (a : as,k,a,k + 1)
  | q /= r = (min q r : as,i,p,k + 1)
  | otherwise = (b : as,k,b,k + 1)
  where a = llcpV xa 0 k
        b = q + llcpV xa q (q + k)
        q = as !! (i - 1)
        r = p - (k - i)

-- | Compute the longest common sequence
-- | Given an array xs and two indices j and k return the longest
-- | common sequence of xs[j] & xs[k]
llcpV :: Eq a
      => V.Vector a -> Int -> Int -> Int
llcpV xs j k
  | j == n || k == n = 0
  | xs V.! j == xs V.! k =
    1 +
    llcpV xs
          (j + 1)
          (k + 1)
  | otherwise = 0
  where n = length xs