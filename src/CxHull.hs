-- | Convex hull
module CxHull where

import Det (det)

type Point = [Integer]

type Simplex = ([Point],Int)

dimension :: Point -> Int
dimension pt = length pt - 1

orientation :: [Point] -> Int
orientation = fromIntegral . signum . det