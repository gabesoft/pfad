-- | Finding a celebrity clique
module CClicque where

subseqs :: [t] -> [[t]]
subseqs [] = [[]]
subseqs (x:xs) = map (x :) (subseqs xs) ++ subseqs xs

ccliques [] = [[]]
ccliques (p:ps) =
  map (p :) (filter (member p ps) css) ++ filter (nonmember p) css
  where css = ccliques ps

member = undefined

nonmember = undefined

cclique :: [a] -> [a]
cclique = foldr op []

op :: a -> [a] -> [a]
op p cs
  | null cs = [p]
  | not (p `knows` c) = [p]
  | not (c `knows` p) = cs
  | otherwise = p : cs
  where c = head cs

knows = undefined