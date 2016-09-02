-- | Schorr-Waite algorithm
module SchWa where

type Node = Int

type Graph = Node -> (Node,Node)

left :: Graph -> Node -> Node
left g n = fst (g n)

right :: Graph -> Node -> Node
right g n = snd (g n)

setl :: Graph -> Node -> Graph
setl g l = \n -> (l,right g n)

setr :: Graph -> Node -> Graph
setr g r = \n -> (left g n,r)

mark :: Graph -> Node -> (Graph,Node -> Bool)
mark g root =
  seek0 (g,const False)
        [root]

seek0 :: (Graph, Node -> Bool) -> [Node] -> (Graph, Node -> Bool)
seek0 (g,m) [] = (g,m)
seek0 (g,m) (x:xs)
  | not (m x) =
    seek0 (g,set m x)
          (left g x : right g x : xs)
  | otherwise = seek0 (g,m) xs

set :: Eq a => (a -> Bool) -> a -> a -> Bool
set f x y
  | y == x = True
  | otherwise = f y

unset :: Eq a => (a -> Bool) -> a -> a -> Bool
unset f x y
  | y == x = False
  | otherwise = f y