-- | Improving on saddleback search
module Saddleback where

-- | Return a list of all pairs (x,y) satisfying f(x,y) = z
-- invert0 (uncurry (+)) 10
invert0 :: (Enum t,Eq t,Num t)
        => ((t,t) -> t) -> t -> [(t,t)]
invert0 f z = [(x,y)|x <- [0 .. z],y <- [0 .. z],f (x,y) == z]

invert1 :: (Enum t,Eq t,Num t)
        => ((t,t) -> t) -> t -> [(t,t)]
invert1 f z = sfind1 (0,z) f z

sfind1
  :: (Enum t,Enum t1,Eq t,Num t1)
  => (t,t1) -> ((t,t1) -> t) -> t -> [(t,t1)]
sfind1 (u,v) f z = [(x,y)|x <- [u .. z],y <- [v,v - 1 .. 0],f (x,y) == z]

invert2 :: (Num a,Ord a)
        => ((a,a) -> a) -> a -> [(a,a)]
invert2 f z = sfind2 (0,z) f z z

sfind2
  :: (Num t,Num a,Ord t,Ord a)
  => (a,t) -> ((a,t) -> a) -> a -> a -> [(a,t)]
sfind2 (u,v) f z n
  | u > n || v < 0 = []
  | z' < z = sfind2 (u + 1,v) f z n
  | z' == z = (u,v) : sfind2 (u + 1,v - 1) f z n
  | z' > z = sfind2 (u,v - 1) f z n
  where z' = f (u,v)

-- | Binary search of g x = z, where g must be an increasing function
--   and g a <= z < g b
bsearch :: (Integral a,Ord a1)
        => (a -> a1) -> (a,a) -> a1 -> a
bsearch g (a,b) z
  | a + 1 == b = a
  | g m <= z = bsearch g (m,b) z
  | otherwise = bsearch g (a,m) z
  where m = (a + b) `div` 2

invert :: Integral a1
        => ((a1,a1) -> a1) -> a1 -> [(a1,a1)]
invert f z = sfind2 (0,m) f z n
  where m =
          bsearch (\y -> f (0,y))
                  (-1,z + 1)
                  z
        n =
          bsearch (\x -> f (x,0))
                  (-1,z + 1)
                  z

-- currently this version fails some tests
invert4 :: Integral t
       => ((t,t) -> t) -> t -> [(t,t)]
invert4 f z =
  sfind4 (0,m)
         (n,0)
         f
         z
  where m =
          bsearch (\y -> f (0,y))
                  (-1,z + 1)
                  z
        n =
          bsearch (\x -> f (x,0))
                  (-1,z + 1)
                  z

sfind4
  :: (Integral r,Ord a)
  => (r,r) -> (r,r) -> ((r,r) -> a) -> a -> [(r,r)]
sfind4 (u,v) (r,s) f z
  | u > r || v < s = []
  | v - s <= r - u =
    rfind $ bsearch (\x -> f (x,q)) (u - 1,r + 1) z
  | otherwise =
    cfind $ bsearch (\y -> f (p,y)) (s - 1,v + 1) z
  where p = (u + r) `div` 2
        q = (v + s) `div` 2
        rfind k =
          if f (k,q) == z
             then (k,q) :
                  sfind4 (u,v)
                         (k - 1,q + 1)
                         f
                         z
             else sfind4 (u,v)
                         (k,q + 1)
                         f
                         z ++
                  sfind4 (k + 1,q - 1)
                         (r,s)
                         f
                         z
        cfind k =
          sfind4 (u,v)
                 (p - 1,k + 1)
                 f
                 z ++
          (if f (p,k) == z
              then (p,k) :
                   sfind4 (p + 1,k - 1)
                          (r,s)
                          f
                          z
              else sfind4 (p + 1,k)
                          (r,s)
                          f
                          z)