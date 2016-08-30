-- | Computing determinants
module Det where

import Control.Monad (replicateM)
import Data.Ratio
import System.Random (randomRIO, Random(..))

m0 :: [[Integer]]
m0 = [[1,2],[3,4]]

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
minors (x:xs) = xs : map (x :) (minors xs)

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs = take n xs : chunks n (drop n xs)

randListX :: (Num a,Random a)
          => a -> a -> a -> Int -> IO [a]
randListX seed l h n =
  replicateM n
             ((* seed) <$> randomRIO (l,h))

randList :: (Num a,Random a)
         => Int -> IO [a]
randList = randListX 1 (-10000) 10000

matrix :: (Num a,Random a)
       => Int -> IO [[a]]
matrix n = chunks n <$> randList (n * n)

mdet :: Num a
     => ([[a]] -> a) -> Int -> IO a
mdet det n =
  do m <- matrix n :: IO [[Integer]]
     return $ det (fmap fromIntegral <$> m)

det1 :: [[Ratio Integer]] -> Ratio Integer
det1 [[x]] = x
det1 xss =
  case break ((/= 0) . head) xss of
    (_,[]) -> 0
    (yss,zs:zss) ->
      if even (length yss)
         then x
         else (-x)
      where x = head zs * det1 (reduce zs (yss ++ zss))

reduce :: (Fractional c,Functor f)
       => [c] -> f [c] -> f [c]
reduce as bss = reduce1 as <$> bss
  where reduce1 (x:xs) (y:ys) = zipWith (\a b -> b - (y / x) * a) xs ys
        reduce1 _ _ = error "reduce1: invalid input"

condense :: Integral a
         => a -> [[a]] -> [[a]]
condense k = map (map det . pair . uncurry zip) . pair
  where pair [] = []
        pair (x:xs) = (,) x <$> xs
        det ((a,b),(c,d)) = (a * d - b * c) `div` k

det2 :: (Integral a,Eq a)
     => [[a]] -> a
det2 [[x]] = x
det2 xss =
  case break ((/= 0) . head) xss of
    (_,[]) -> 0
    (yss,zs:zss) ->
      if even (length yss)
         then y
         else (-y)
      where x = det2 . condense 1 $ (zs : yss ++ zss)
            d = head zs ^ (length xss - 2)
            y = x `div` d

det3 :: (Integral a,Eq a)
     => [[a]] -> a
det3 = det 1
  where det _ [[x]] = x
        det k xss =
          case break ((/= 0) . head) xss of
            (_,[]) -> 0
            (yss,zs:zss) ->
              let x =
                    det (head zs)
                        (condense k (zs : yss ++ zss))
              in if even (length yss)
                    then x
                    else (-x)