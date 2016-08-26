-- | Solving the rush hour problem
module Rhp where

import Data.List ((\\))
import qualified Data.Set as Set

-- | Breath first search (list implementation)
bfs
  :: Eq state
  => [state] -> [([move],state)] -> Maybe [move]
bfs _ [] = Nothing
bfs seen (p@(ms,q):ps)
  | solved q = Just ms
  | q `elem` seen = bfs seen ps
  | otherwise =
    bfs (q : seen)
        (ps ++ succs p)
  where succs (ms,q) = [(ms ++ [m],move q m)|m <- moves q]

solved = undefined

move = undefined

moves = undefined

-- Grid representation
-- exit cell = 20
--  1  2  3  4  5  6
--  8  9 10 11 12 13
-- 15 16 17 18 19 20 <-
-- 22 23 24 25 26 27
-- 29 30 31 32 33 34
-- 36 37 38 39 40 41
sample1 :: Grid
sample1 =
  [(17,18) -- special
  ,(1,15)
  ,(2,9)
  ,(3,10)
  ,(4,11)
  ,(5,6)
  ,(12,19)
  ,(13,27)
  ,(24,26)
  ,(31,38)
  ,(33,34)
  ,(36,37)
  ,(40,41)]

type Cell = Int

type Grid = [(Cell,Cell)]

type Vehicle = Int

type Move = (Vehicle,Cell)

type State = Grid

occupied :: Grid -> [Cell]
occupied = foldr (merge . fillcells) []

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
  | x <= y = x : merge xs (y : ys)
  | otherwise = y : merge (x : xs) ys

fillcells :: (Enum t, Num t, Ord t) => (t, t) -> [t]
fillcells (r,f)
  | r > f - 7 = [r .. f]
  | otherwise = [r,r + 7 .. f]