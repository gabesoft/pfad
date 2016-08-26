-- | Solving the rush hour problem
module Rhp where

import Data.List ((\\))
import qualified Data.Set as Set

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

type Path = ([Move],State)

type Plan = [Move]

type APath = ([Move],State,Plan)

horiz :: (Num a, Ord a) => a -> a -> Bool
horiz r f = r > f - 7

occupied :: Grid -> [Cell]
occupied = foldr (merge . fillcells) []

merge :: Ord a
      => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
  | x <= y = x : merge xs (y : ys)
  | otherwise = y : merge (x : xs) ys

allcells :: [Cell]
allcells = [c | c <- [1 .. 41],c `mod` 7 /= 0]

fillcells :: (Enum t,Num t,Ord t)
          => (t,t) -> [t]
fillcells (r,f)
  | horiz r f= [r .. f]
  | otherwise = [r,r + 7 .. f]

freecells :: Grid -> [Cell]
freecells g = allcells \\ occupied g

moves :: Grid -> [Move]
moves g = [(v,c) | (v,i) <- zip [0 ..] g,c <- adjs i,c `elem` fs]
  where fs = freecells g

adjs :: (Cell,Cell) -> [Cell]
adjs (r,f)
  | horiz r f = [f + 1,r - 1]
  | otherwise = [f + 7,r - 7]

move :: Grid -> Move -> Grid
move g (v,c) = g1 ++ adjust pos c : g2
  where (g1, pos : g2) = splitAt v g

adjust :: (Num a, Ord a) => (a, a) -> a -> (a, a)
adjust (r,f) c
  | horiz r f = if c > f then (r + 1, c) else (c, f - 1)
  | otherwise = if c < r then (c, f - 7) else (r + 7, c)

solved :: Grid -> Bool
solved g = snd (head g) == 20

bfs :: Set.Set State -> [[Path]] -> [Path] -> Maybe [Move]
bfs _ [] [] = Nothing
bfs seen pss [] = bfs seen [] (concat $ reverse pss)
bfs seen pss ((ms,st) : ps)
  | solved st = Just ms
  | Set.member st seen = bfs seen pss ps
  | otherwise = bfs (Set.insert st seen) (succs (ms,st) : pss) ps

succs :: Path -> [Path]
succs (ms, st) = [(ms ++ [m], move st m) | m <- moves st]

bsolve :: State -> Maybe [Move]
bsolve g = bfs Set.empty [] [([],g)]

goalmoves :: Grid -> Plan
goalmoves g = [(0,c) | c <- [snd (head g) + 1..20]]

blocker :: Grid -> Cell -> (Vehicle,(Cell,Cell))
blocker grid = search (zip [0 ..] grid)
  where search ((v,i):vis) c =
          if covers c i
             then (v,i)
             else search vis c
        covers c (r,f) =
          r <= c && c <= f && (horiz r f || (c - r) `mod` 7 == 0)

freeingmoves
  :: Cell -> (Vehicle,(Cell,Cell)) -> [Plan]
freeingmoves c (v,(r,f))
  | r > f - 7 =
    [[(v,j)|j <- [f + 1 .. c + n]]|c + n < k + 7] ++
    [[(v,j)|j <- [r - 1,r - 2 .. c - n]]|c - n > k]
  | otherwise =
    [[(v,j)|j <- [r - 7,r - 14 .. c - m]]|c - m > 0] ++
    [[(v,j)|j <- [f + 7,f + 14 .. c + m]]|c + m < 42]
  where (k,m,n) = (f - f `mod` 7,f - r + 7,f - r + 1)

premoves :: Grid -> Move -> [Plan]
premoves g (_,c) = freeingmoves c (blocker g c)

newplans :: Grid -> Plan -> [Plan]
newplans _ [] = []
newplans g (mv:mvs) = mkplans (expand g mv ++ mvs)
  where mkplans ms
          | m `elem` gms = [ms]
          | otherwise =
            concat [mkplans (pms ++ ms)|pms <- premoves g m,all (`elem` ms) pms]
          where m = head ms
        gms = moves g

expand :: Grid -> Move -> [Move]
expand g (v,c)
  | r > f - 7 =
    if c > f
       then [(v,p)|p <- [f + 1 .. c]]
       else [(v,p)|p <- [r - 1,r - 2 .. c]]
  | otherwise =
    if c > f
       then [(v,p)|p <- [f + 7,f + 14 .. c]]
       else [(v,p)|p <- [r - 7,r - 14 .. c]]
  where (r,f) = g !! v

asuccs :: APath -> [APath]
asuccs (ms,q,plan) = [(ms ++ [m],move q m,p) | m:p <- newplans q plan]

bsuccs :: APath -> [APath]
bsuccs (ms,q,_) = [(ms ++ [m],q', goalmoves q') | m <- moves q, let q' = move q m]

psearch
  :: Set.Set State -> [APath] -> [APath] -> Maybe [Move]
psearch _ [] [] = Nothing
psearch seen rs [] = psearch seen [] rs
psearch seen rs (p@(ms,q,plan):ps)
  | solved q = Just (reverse ms)
  | Set.member q seen = psearch seen rs ps
  | otherwise =
    psearch (Set.insert q seen)
            (bsuccs p ++ rs)
            (asuccs p ++ ps)

psolve :: Grid -> Maybe [Move]
psolve g = psearch Set.empty [] [([], g, goalmoves g)]