-- | Convex hull
module CxHull where

import Data.List ((\\), sort)
import Det (det)

type Point = [Integer]

type Simplex = ([Point],Int)

type Facet = ([Point],Int)

dimension :: Point -> Int
dimension pt = length pt - 1

orientation :: [Point] -> Int
orientation = fromIntegral . signum . det

facets :: Simplex -> [Facet]
facets (us,b) =
  zip (minors us)
      (cycle [b,-b])

minors :: [a] -> [[a]]
minors [] = []
minors (x:xs) = xs : map (x :) (minors xs)

insideCS :: Simplex -> Point -> Bool
insideCS smp p = and [0 <= b * orientation (p : us)|(us,b) <- facets smp]

insideCH0 :: [Point] -> Point -> Bool
insideCH0 vs p = or [insideCS smp p|smp <- simplexes vs]

simplexes :: [Point] -> [Simplex]
simplexes vs =
  [(us,b)
  |us <- tuples (dimension (head vs) + 1) vs
  ,let b = orientation us
  ,b /= 0]

insideCH1 :: [Point] -> Point -> Bool
insideCH1 vs p = or [insideCS smp p|smp <- partition vs]

partition :: [Point] -> [Simplex]
partition vs =
  case findSimplex vs of
    Nothing -> []
    Just smp ->
      foldl update
            [smp]
            (vs \\ vertices smp)

vertices :: Simplex -> [Point]
vertices = sort . fst

findSimplex0 :: [Point] -> Maybe Simplex
findSimplex0 vs
  | null smps = Nothing
  | otherwise = Just (head smps)
  where smps = simplexes vs

findSimplex :: [Point] -> Maybe Simplex
findSimplex [] = Nothing
findSimplex (x:xs) =
  search (length x - 1)
         1
         [x]
         xs
  where search d k us vs
          | k == d + 1 = Just (us,orientation us)
          | null vs = Nothing
          | degenerate k
                       (head vs : us) = search d k us (tail vs)
          | otherwise =
            search d
                   (k + 1)
                   (head vs : us)
                   (tail vs)

degenerate :: Integral a
           => Int -> [[a]] -> Bool
degenerate k = all (== 0) . map det . submatrices k . transpose

update :: [Simplex] -> Point -> [Simplex]
update smps v =
  smps ++
  map (newSimplex v)
      (visible v (external smps))

transpose :: [[a]] -> [[a]]
transpose [] = []
transpose ([]:_) = []
transpose xss = (head <$> xss) : transpose (tail <$> xss)

submatrices :: Int -> [a] -> [[a]]
submatrices k vs =
  map (++ [last vs])
      (tuples k (init vs))

tuples :: Int -> [a] -> [[a]]
tuples 0 _ = [[]]
tuples _ [] = []
tuples n (x:xs) = map (x :) (tuples (n - 1) xs) ++ tuples n xs

subseqs :: [t] -> [[t]]
subseqs [] = [[]]
subseqs (x:xs) = map (x :) (subseqs xs) ++ subseqs xs

external :: [Simplex] -> [Facet]
external = foldr op [] . sort . concatMap facets
  where op smp [] = [smp]
        op smp (smp':smps)
          | vertices smp == vertices smp' = smps
          | otherwise = smp : smp' : smps

visible :: Point -> [Facet] -> [Facet]
visible v fs = [(us,b)|(us,b) <- fs,b * orientation (v : us) < 0]

newSimplex :: Point -> Facet -> Simplex
newSimplex v (us,b) = (v : us,-b)