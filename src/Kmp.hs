-- | Knuth-Morris-Pratt algorithm
module Kmp (matches0, matches1, matches2, matches) where

import Data.List (tails, inits)

data Rep a
  = Null
  | Node a
         (Rep a)
         (Rep a)

matches0 :: Eq a
         => [a] -> [a] -> [Int]
matches0 ws = map length . filter (endswith ws) . inits

matches1 :: (Eq a,Num a1)
         => [a] -> [a] -> [a1]
matches1 ws = map fst . filter (null . snd . snd) . scanl step (0,([],ws))
  where step (n,(us,vs)) x = (n + 1,op (us,vs) x)
        op (us,vs) x
          | prefix [x] vs = (us ++ [x],tail vs)
          | null us = ([],ws)
          | otherwise = op (split ws (tail us)) x
        split ws = foldl op ([],ws)

endswith :: Eq a
         => [a] -> [a] -> Bool
endswith ws =
  not . null . filter (ws `prefix`) . (filter $ (== n) . length) . tails
  where n = length ws

prefix :: Eq a
       => [a] -> [a] -> Bool
prefix [] _ = True
prefix (_:_) [] = False
prefix (u:us) (v:vs) = u == v && prefix us vs

matches2 :: (Eq t,Num a)
         => [t] -> [t] -> [a]
matches2 ws =
  map fst . filter (null . snd . abst . snd) . scanl step (0,repr ([],ws))
  where step (n,r) x = (n + 1,op r x)
        abst Null = ([],ws)
        abst (Node (us,vs) _ _) = (us,vs)
        repr (us,vs) =
          Node (us,vs)
               (left us vs)
               (right us vs)
        left [] _ = Null
        left (_:us) _ = repr (split ws us)
        right _ [] = Null
        right us (v:vs) = repr (us ++ [v],vs)
        split ws = abst . foldl op (repr ([],ws))
        root = repr ([],ws)
        op Null _ = root
        op (Node (_,vs) l r) x
          | prefix [x] vs = r
          | otherwise = op l x

matches :: (Eq a)
        => [a] -> [a] -> [Int]
matches [] = \xs -> [0 .. length xs]
matches ws = map fst . filter (ok . snd) . scanl step (0,root)
  where ok (Node vs _ _) = null vs
        ok Null = error "malfunction"
        step (n,t) x = (n + 1,op t x)
        op Null _ = root
        op (Node [] l _) x = op l x
        op (Node (v:_) l r) x =
          if v == x
             then r
             else op l x
        root = grep Null ws
        grep l [] = Node [] l Null
        grep l (v:vs) =
          Node (v : vs)
               l
               (grep (op l v) vs)