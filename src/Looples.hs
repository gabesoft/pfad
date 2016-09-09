-- | Looples algorithms
module Looples where

import Control.Arrow

unfoldr :: (b -> Maybe (a,b)) -> b -> [a]
unfoldr step b =
  case step b of
    Just (a,c) -> a : unfoldr step c
    Nothing -> []

uncons :: [a] -> Maybe (a,[a])
uncons [] = Nothing
uncons (x:xs) = Just (x,xs)

lreverse :: [a] -> [a]
lreverse = unfoldr uncons . foldl (flip (:)) []

lconcat :: [[a]] -> [a]
lconcat = unfoldr step . filter (not . null)
  where step [] = Nothing
        step ((x:xs):xss) = Just (x,consl xs xss)

consl :: [t] -> [[t]] -> [[t]]
consl [] xss = xss
consl xs xss = xs : xss

type Forest a = [Rose a]

data Rose a =
  Node a
       (Forest a)
  deriving (Eq,Show)

-- preorder [mkbtree [7,5,9,13,2,10,1,8,3]]
preorder :: Forest a -> [a]
preorder = unfoldr step . wrapl
  where wrapl xs = consl xs []
        step [] = Nothing
        step ((Node x xs:ys):zss) = Just (x,consl xs (consl ys zss))

mkbtree :: Ord a
        => [a] -> Rose a
mkbtree [] = error "At least a root node is required"
mkbtree (x:xs) = last $ unfoldr step (Node x [],xs)
  where step (_,[]) = Nothing
        step (t,y:ys) = Just (u,(u,ys))
          where u = insert t y
        insert (Node n []) y = Node n [Node y []]
        insert (Node n [m@(Node k _)]) y
          | y < n && n < k = Node n [Node y [],m]
          | y > n && n > k = Node n [m,Node y []]
          | otherwise = Node n [insert m y]
        insert (Node n [l,r]) y
          | y < n = Node n [insert l y,r]
          | otherwise = Node n [l,insert r y]

box :: [a] -> [a] -> [a]
box xs ys = mix xs (ys,reverse ys)

boxall :: [[a]] -> [a]
boxall = foldr box []

mix :: [a] -> ([a],[a]) -> [a]
mix [] (ys,_) = ys
mix (x:xs) (ys,sy) = ys ++ [x] ++ mix xs (sy,ys)

jcode :: Int -> [Int]
jcode 1 = []
jcode n =
  box (bumpBy 1 (jcode (n - 1)))
      [n - 1,n - 2 .. 1]
  where bumpBy _ [] = []
        bumpBy k [a] = [a + k]
        bumpBy k (a:b:as) = (a + k) : b : bumpBy k as

split :: Arrow a
      => a b (b,b)
split = arr id &&& arr id

unsplit :: Arrow a
        => (b -> c -> d) -> a (b,c) d
unsplit = arr . uncurry

liftArr2
  :: Arrow cat => (c1 -> c2 -> c) -> cat a c1 -> cat a c2 -> cat a c
liftArr2 op f g = split >>> first f >>> second g >>> unsplit op