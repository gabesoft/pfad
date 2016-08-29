-- | Computing determinants
module Det where

import Control.Monad (replicateM)
import Data.Ratio
import System.Random (randomRIO)

m1 :: [[Integer]]
m1 = [[1 .. 3],[4 .. 6],[7 .. 9]]

m2 :: [[Integer]]
m2 = [[-2,2,-3],[-1,1,3],[2,0,-1]]

m3 :: [[Integer]]
m3 = [[1 .. 4],[5 .. 8],[9 .. 12],[14 .. 17]]

m4 :: [[Integer]]
m4 = [[1 .. 4],[5 .. 8],[9 .. 12],[14 .. 17]]

m5 :: [[Integer]]
m5 =
  [[790,-786,-12,-69,-757,-783,911,588,-375,-578]
  ,[89,-528,111,879,-194,-429,5,241,-712,-155]
  ,[-565,-161,-13,936,369,-128,-305,612,-392,465]
  ,[-797,-179,876,-687,802,174,-496,-926,-802,93]
  ,[496,261,279,-304,697,-560,-817,299,-851,911]
  ,[497,-971,451,785,278,846,-278,72,-444,859]
  ,[283,571,-531,-427,641,-706,-942,182,-407,-608]
  ,[-616,-626,-323,9,958,-283,61,-481,-750,-517]
  ,[141,166,174,-638,782,-925,-494,-306,926,-735]
  ,[632,307,-243,779,407,-320,-302,934,-905,894]]

m6 :: [[Ratio Integer]]
m6 = fmap fromIntegral <$> m5

-- | Calculates the determinant of a matrix using the school-book method
det0 :: Num a
     => [[a]] -> a
det0 [[x]] = x
det0 xss = foldr1 (-) (zipWith (*) col1 (det0 <$> minors cols))
  where col1 = map head xss
        cols = map tail xss

-- | Return the sub-sequences of the input list in which
-- | just one argument is dropped
minors :: [t] -> [[t]]
minors [] = []
minors [x,y] = [[x],[y]]
minors (x:xs) = map (x :) (minors xs) ++ [xs]

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs = take n xs : chunks n (drop n xs)

randListX :: Int -> Int -> Int -> Int -> IO [Int]
randListX seed l h n =
  replicateM n
             ((* seed) <$> randomRIO (l,h))

randList :: Int -> IO [Int]
randList = randListX 1 (-1000) 1000

matrix :: Int -> IO [[Int]]
matrix n = chunks n <$> randList (n * n)

mdet :: ([[Int]] -> r) -> Int -> IO r
mdet det n =
  do m <- matrix n
     return $ det m

det1 :: [[Ratio Integer]] -> Ratio Integer
det1 [[x]] = x
det1 xss =
  case break ((/= 0) . head) xss of
    (yss,[]) -> 0
    (yss,zs:zss) ->
      if even (length yss)
         then x
         else (-x)
      where x = head zs * det1 (reduce zs (yss ++ zss))

reduce :: (Fractional c, Functor f) => [c] -> f [c] -> f [c]
reduce as bss = reduce1 as <$> bss
  where reduce1 (x:xs) (y:ys) = zipWith (\a b -> b - d * a) xs ys
          where d = y / x