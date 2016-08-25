-- | Knuth-Morris-Pratt algorithm
module Kmp where

import Data.List (tails, inits)

data Rep a
  = Null
  | Node a
         (Rep a)
         (Rep a)

matches0 :: Eq a
         => [a] -> [a] -> [Int]
matches0 _ [] = []
matches0 ws xs = map length . filter (endswith ws) . inits $ xs

matches1 :: (Eq a
            ,Num a1)
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

-- | Abstraction function
tabs (Node (us,vs) _ _) = (us,vs)

-- | Representation function
trep (us,vs) =
  Node (us,vs)
       (left us vs)
       (right us vs)
  where left [] _ = Null
        left (_:us) vs = trep (split (us ++ vs) us)
        right _ [] = Null
        right us (v:vs) = trep (us ++ [v],vs)
        split ws = tabs . foldl op (trep ([],ws))
        root = trep ([],(us ++ vs))
        op Null x = root
        op (Node (us,vs) l r) x
          | prefix [x] vs = r
          | otherwise = op l x