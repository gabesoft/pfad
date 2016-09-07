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
  :: (Graph -> Node -> (Graph,Node -> Bool)) -> [(Node,Bool)]
sampleReach mark = zip ns rs
  where ns = [0 .. 13]
        rs = snd (mark sample 0) <$> ns

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

setl :: Graph -> Node -> Node -> Graph
setl g x y =
  Map.insert x
             (Just y,right g x)
             g

setr :: Graph -> Node -> Node -> Graph
setr g x y =
  Map.insert x
             (left g x,Just y)
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
  | isNothing x && null xs = (g,m)
  | isNothing x = seek1 (g,m) (right g (head xs)) (tail xs)
  | not (m $ x')  =  seek1 (g, set m x') (left g x') (x' : xs)
  | null xs = (g,m)
  | otherwise = seek1 (g,m) (right g (head xs)) (tail xs)
  where x' = fromJust x

set :: Eq a
    => (a -> Bool) -> a -> a -> Bool
set f x y = replace f x True y

unset :: Eq a
      => (a -> Bool) -> a -> a -> Bool
unset f x y
  | y == x = False
  | otherwise = f y

replace :: Eq a
        => (a -> b) -> a -> b -> (a -> b)
replace f x y z =
  if z == x
     then y
     else f z