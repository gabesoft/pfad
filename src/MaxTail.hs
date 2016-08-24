-- | Computing the largest tail of a list
module MaxTail where

maxtail :: Ord a
        => [a] -> [a]
maxtail [] = []
maxtail xs = go (head xs,tail xs,xs)
  where go (_,[],ts) = ts
        go (x,y:ys,ts)
          | x < y = go (y,ys,y : ys)
          | otherwise = go (x,ys,ts)

-- currently returns incorrect results for inputs like "zazb"
maxtail1 :: Ord a
         => [a] -> [a]
maxtail1 [] = []
maxtail1 (x:xs) = step (0,1,x : xs,x : xs,xs)

step :: Ord a
     => (Int,Int,[a],[a],[a]) -> [a]
step (_,_,ys,_,[]) = ys
step (p,q,ys,w:ws,x:xs)
  | w < x =
    maxtail (drop (q - r)
                  (w : ws))
  | w == x = step (p + 1,q,ys,ws,xs)
  | w > x = step (0,p + q + 1,ys,ys,xs)
  where r = p `mod` q