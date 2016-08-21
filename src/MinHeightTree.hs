-- | Building a tree with minimum height
module MinHeightTree where

import Data.Foldable
import Data.Function (on)

data Tree a
  = Leaf a
  | Fork (Tree a)
         (Tree a)
  deriving (Eq,Show,Ord)

type Forest a = [Tree a]

cost :: (Num a,Ord a)
     => Tree a -> a
cost (Leaf x) = x
cost (Fork u v) = 1 + (cost u `max` cost v)

mincostTree0 :: (Num a,Ord a)
             => [a] -> Tree a
mincostTree0 = minimumBy (compare `on` cost) . trees0

trees0 :: [a] -> [Tree a]
trees0 [x] = [Leaf x]
trees0 (x:xs) =
  concatMap (prefixes0 x)
            (trees0 xs)

prefixes0 :: a -> Tree a -> [Tree a]
prefixes0 x t@(Leaf _) = [Fork (Leaf x) t]
prefixes0 x t@(Fork left right) =
  Fork (Leaf x) t : [Fork l right|l <- prefixes0 x left]

foldrn :: (a -> b -> b) -> (a -> b) -> [a] -> b
foldrn _ _ [] = error "Non empty list expected"
foldrn _ g [x] = g x
foldrn f g (x:xs) = f x (foldrn f g xs)

trees1 :: [a] -> [Tree a]
trees1 =
  foldrn (concatMap . prefixes0)
         ((: []) . Leaf)

rollup :: Forest a -> Tree a
rollup [] = error "Non empty forest expected"
rollup (x:xs) = foldl Fork x xs

prefixes1 :: a -> Forest a -> [Forest a]
prefixes1 x ts =
  [Leaf x : rollup (take k ts) : drop k ts|k <- [1 .. length ts]]

forests :: [a] -> [Forest a]
forests =
  foldrn (concatMap . prefixes1)
         ((: []) . (: []) . Leaf)

trees2 :: [a] -> [Tree a]
trees2 = map rollup . forests

minBy :: Ord a
      => (t -> a) -> [t] -> t
minBy _ [] = error "Non empty list expected"
minBy _ [x] = x
minBy f (x:xs) = foldl (cmp f) x xs

cmp :: Ord a
    => (t -> a) -> t -> t -> t
cmp f u v =
  if f u <= f v
     then u
     else v

insert0 :: (Num a,Ord a)
        => a -> [Tree a] -> [Tree a]
insert0 x ts = Leaf x : split0 x ts

split0 :: (Num a,Ord a)
       => a -> [Tree a] -> [Tree a]
split0 x [u] = [u]
split0 x (u:v:ts) =
  if x `max` cost u < cost v
     then u : v : ts
     else split0 x (Fork u v : ts)

fork :: (Num t,Ord t)
     => (t,Tree a) -> (t,Tree a) -> (t,Tree a)
fork (a,u) (b,v) = (1 + a `max` b,Fork u v)

leaf :: a -> (a,Tree a)
leaf x = (x,Leaf x)

split :: (Num a,Ord a)
      => a -> [(a,Tree a)] -> [(a,Tree a)]
split x [u] = [u]
split x (u:v:ts) =
  if x `max` fst u < fst v
     then u : v : ts
     else split x (fork u v : ts)

insert :: (Num a,Ord a)
       => a -> [(a,Tree a)] -> [(a,Tree a)]
insert x ts = leaf x : split x ts

mincostTree :: (Num a,Ord a)
            => [a] -> Tree a
mincostTree = foldl1 Fork . map snd . foldrn insert ((: []) . leaf)