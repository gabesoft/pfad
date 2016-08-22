-- | Ranking the suffixes (tails) of a list
module RankTails where

import Data.Array
import Data.List (sortBy)
import qualified Data.Map.Strict as Map

rank0 :: Ord a
      => [a] -> [Int]
rank0 xs = (\x -> length $ filter (< x) xs) <$> xs

rank1 :: Ord a
      => [a] -> [Int]
rank1 xs = findRank (ranks Map.empty ys) <$> xs
  where ys = sortBy (flip compare) xs
        ranks ord [] = ord
        ranks ord (z:zs) =
          ranks (Map.insert z
                            (length rest)
                            ord)
                rest
          where rest = dropWhile (== z) zs
        findRank ord x = Map.findWithDefault 0 x ord

ranktails0 :: Ord a
           => [a] -> [Int]
ranktails0 = rank0 . tails

tails :: [a] -> [[a]]
tails [] = []
tails xs = xs : tails (tail xs)

(<<) :: Ord a
     => [a] -> [a] -> [Int]
(<<) xs ys = rank0 (zip xs ys)

rats :: Ord a
     => Int -> [a] -> [Int]
rats k = rank0 . map (take k) . tails

ranktails1 :: Ord a
           => [a] -> [Int]
ranktails1 xs = rats (length xs) xs

ranktails2 :: Ord a
           => [a] -> [Int]
ranktails2 = applyUntil isPerm rerankings . rank0

rerankings :: [[Int] -> [Int]]
rerankings = rerank <$> iterate (* 2) 1

rerank :: (Enum a,Num a,Ord a)
       => Int -> [a] -> [Int]
rerank k rs = rs << shiftBy k rs

shiftBy :: (Num a,Enum a)
        => Int -> [a] -> [a]
shiftBy k rs
  | k == 0 = rs
  | otherwise = map (+ m) (drop k rs) ++ [m - 1,m - 2 .. 0]
  where m = fromIntegral k

applyUntil :: (a -> Bool) -> [a -> a] -> a -> a
applyUntil p [] x =
  if p x
     then x
     else error "No more functions to apply"
applyUntil p (f:fs) x =
  if p x
     then x
     else applyUntil p
                     fs
                     (f x)

isPerm :: [Int] -> Bool
isPerm xs =
  and (elems (accumArray (||)
                         False
                         (0,n - 1)
                         (zip xs (repeat True))))
  where n = length xs

ssnoc :: a -> [a] -> [a]
ssnoc x xs = xs ++ [x]

slift :: Num a
      => [a] -> [a]
slift = ssnoc 0 . map (+ 1)

sshift :: [Integer] -> [Integer]
sshift = slift . tail

psort :: Ord b
      => [(a,b)] -> [[a]]
psort xys = pass xys []

pass :: Ord b
     => [(a,b)] -> [[a]] -> [[a]]
pass [] xss = xss
pass (e@(x,y):xys) xss = step xys [] [x] [] xss
  where step [] as bs cs xss = pass as (bs : pass cs xss)
        step (e@(x,y'):xys) as bs cs xss
          | y' < y = step xys (e : as) bs cs xss
          | y' == y = step xys as (x : bs) cs xss
          | y' > y = step xys as bs (e : cs) xss

label :: [[a]] -> [[(a,Int)]]
label xss = zipWith tag xss (scanl (+) 0 (map length xss))
  where tag xs k = [(x,k)|x <- xs]

resort :: [(Int,a)] -> [a]
resort ijs = elems (array (0,length ijs - 1) ijs)

rank2 :: Ord a
      => [a] -> [Int]
rank2 = resort . concat . label . psort . zip [0 ..]

rank :: Ord a
     => [a] -> [Int]
rank = rank1