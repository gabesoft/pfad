-- | Schorr-Waite algorithm
module SchWa where

import Control.Monad (join)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, isNothing, fromJust)

type Node = Int

type Graph = Map.Map Node (Maybe Node,Maybe Node)

mkGraph :: Graph
mkGraph = Map.empty

sampleReach
  :: (Graph -> Node -> (Graph,Node -> Bool)) -> Node -> [(Node,Bool)]
sampleReach mark root = zip ns rs
  where ns = [0 .. 13]
        rs = snd (mark sample root) <$> ns

verifyAll :: Bool
verifyAll = all id (check <$> ns)
  where ns = [0 .. 13]
        check x =
          foldr (step x)
                True
                [mark1,mark2,mark]
        step x m acc = acc && sampleReach m x == sampleReach mark0 x

sample :: Graph
sample = Map.fromList xs
  where xs =
          [(0,(Just 1,Just 2))
          ,(1,(Just 3,Just 4))
          ,(2,(Just 7,Just 9))
          ,(4,(Just 9,Nothing))
          ,(5,(Just 3,Just 7))
          ,(7,(Just 3,Just 4))
          ,(8,(Just 1,Nothing))
          ,(9,(Just 10,Just 2))
          ,(11,(Just 10,Just 9))
          ,(13,(Just 6,Just 8))]

left :: Graph -> Node -> Maybe Node
left g n = join $ fst <$> Map.lookup n g

right :: Graph -> Node -> Maybe Node
right g n = join $ snd <$> Map.lookup n g

setl :: Graph -> Node -> Maybe Node -> Graph
setl g x y =
  Map.insert x
             (y,right g x)
             g

setr :: Graph -> Node -> Maybe Node -> Graph
setr g x y =
  Map.insert x
             (left g x,y)
             g

mark0 :: Graph -> Node -> (Graph,Node -> Bool)
mark0 g root =
  seek0 (g,const False)
        [root]

seek0
  :: (Graph,Node -> Bool) -> [Node] -> (Graph,Node -> Bool)
seek0 (g,m) [] = (g,m)
seek0 (g,m) (x:xs)
  | not (m x) =
    seek0 (g,set m x)
          (catMaybes [left g x,right g x] ++ xs)
  | otherwise = seek0 (g,m) xs

mark1 :: Graph -> Node -> (Graph,Node -> Bool)
mark1 g root =
  seek1 (g,const False)
        (Just root)
        []

seek1
  :: (Graph,Node -> Bool) -> Maybe Node -> [Node] -> (Graph,Node -> Bool)
seek1 (g,m) x xs
  | isNothing x =
    if null xs
       then (g,m)
       else next
  | not (m x') =
    seek1 (g,set m x')
          (left g x')
          (x' : xs)
  | null xs = (g,m)
  | otherwise = next
  where x' = fromJust x
        next =
          seek1 (g,m)
                (right g (head xs))
                (tail xs)

mark2 :: Graph -> Node -> (Graph,Node -> Bool)
mark2 g root =
  seek2 (g,const False)
        (const False)
        (Just root)
        []

seek2 :: (Graph,Node -> Bool)
      -> (Node -> Bool)
      -> Maybe Node
      -> [Node]
      -> (Graph,Node -> Bool)
seek2 (g,m) p x xs
  | isNothing x = find2 (g,m) p xs
  | not (m x') =
    seek2 (g,set m x')
          (set p x')
          (left g x')
          (x' : xs)
  | otherwise = find2 (g,m) p xs
  where x' = fromJust x

find2 :: (Graph,Node -> Bool)
      -> (Node -> Bool)
      -> [Node]
      -> (Graph,Node -> Bool)
find2 (g,m) _ [] = (g,m)
find2 (g,m) p (y:ys)
  | not (p y) = find2 (g,m) p ys
  | otherwise =
    seek2 (g,m)
          (unset p y)
          (right g y)
          (y : ys)

mark :: Graph -> Node -> (Graph, Node -> Bool)
mark g root =
  seek (g,const False)
       (const False)
       (Just root)
       Nothing

seek
  :: (Graph, Node -> Bool)
     -> (Node -> Bool)
     -> Maybe Node
     -> Maybe Node
     -> (Graph, Node -> Bool)
seek (g,m) p x y
  | isNothing x = find (g,m) p x y
  | not (m x') =
    seek (setl g x' y,set m x')
         (set p x')
         (left g x')
         x
  | otherwise = find (g,m) p x y
  where x' = fromJust x

find
  :: (Graph, Node -> Bool)
     -> (Node -> Bool)
     -> Maybe Node
     -> Maybe Node
     -> (Graph, Node -> Bool)
find (g,m) p x y
  | isNothing y = (g,m)
  | p y' =
    seek (swing g y' x,m)
         (unset p y')
         (right g y')
         y
  | otherwise =
    find (setr g y' x,m)
         p
         y
         (right g y')
  where swing g y x =
          setr (setl g y x)
               y
               (left g y)
        y' = fromJust y

set :: Eq a
    => (a -> Bool) -> a -> a -> Bool
set f x y = replace f x True y

unset :: Eq a
      => (a -> Bool) -> a -> a -> Bool
unset f x y = replace f x False y

replace :: Eq a
        => (a -> b) -> a -> b -> (a -> b)
replace f x y z =
  if z == x
     then y
     else f z