-- | The Boyer-Moore algorithm
module BoyerMoore where

import Data.List (inits)

matches0 :: Eq a
         => [a] -> [a] -> [Int]
matches0 ws [] = []
matches0 ws xs = map length . filter (endswith ws) . inits $ xs

matches1 :: Eq a
         => [a] -> [a] -> [Int]
matches1 ws = map fst . filter snd . (map . fork) (length,endswith ws) . inits

matches2 :: (Eq a, Num a1) => [a] -> [a] -> [a1]
matches2 ws = map fst . filter ((sw `prefix`) . snd) . scanl step (0,[])
  where sw = reverse ws
        step (n,sx) x = (n + 1,x : sx)

endswith :: Eq a
         => [a] -> [a] -> Bool
endswith ws = (reverse ws `prefix`) . reverse

fork :: (t2 -> t,t2 -> t1) -> t2 -> (t,t1)
fork (f,g) x = (f x,g x)

prefix :: Eq a
       => [a] -> [a] -> Bool
prefix [] _ = True
prefix (_:_) [] = False
prefix (u:us) (v:vs) = u == v && prefix us vs

-- | Compute the longest common sequence
llcp :: (Eq a1,Num a)
     => [a1] -> [a1] -> a
llcp _ [] = 0
llcp [] _ = 0
llcp (x:xs) (y:ys)
  | x == y = 1 + llcp xs ys
  | otherwise = 0

-- shift sw i = head [k | k <- [1..m], llcp sw (drop k sw) == min i (m - k)]
-- where m = length sw
matches3 :: Eq a => [a] -> [a] -> [Int]
matches3 [] = \xs -> [1..length xs]
matches3 ws = test m . scanl step (0,[])
  where (sw,m) = (reverse ws,length ws)
        test _ [] = []
        test j ((n,sx):nsx)
          | i == m = n : test k (drop (k - 1) nsx)
          | otherwise = test m (drop (k - 1) nsx)
          where i' = llcp sw (take j sx)
                i = if i' == j then m else i'
                k = shift i
        shift i = head [k | k <- [1 .. m],llcp sw (drop k sw) == min i (m - k)]
        step (n,sx) x = (n + 1,x : sx)

matches :: Eq a => [a] -> [a] -> [Int]
matches = matches3