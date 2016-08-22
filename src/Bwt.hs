-- | The Burrows-Wheeler transform
module Bwt where

import Data.Array
import Data.List (sort, sortBy, transpose)

-- | Perform the Burrows-Wheeler transform
transform :: Ord a
          => [a] -> ([a],Int)
transform xs = (last <$> xss,position xs xss)
  where xss = sort (rots xs)

-- transform xs = ([xa ! (pa ! i) | i <- [0..n-1]],k)
--   where n = length xs
--         k = length (takeWhile (/= 0) ps)
--         xa = listArray (0,n-1) (rrot xs)
--         pa = listArray (0,n-1) ps
--         ps = snd <$> sort (zip $ tails (tag xs) [0..n-1])

-- tag xs = xs ++ [EOF]

position :: Eq a
         => a -> [a] -> Int
position xs = length . takeWhile (/= xs)

rots :: [a] -> [[a]]
rots xs =
  take (length xs)
       (iterate lrot xs)
  where lrot [] = []
        lrot (y:ys) = ys ++ [y]

takeCols :: Int -> [[a]] -> [[a]]
takeCols j = map (take j)

recreate :: (Ord a)
         => Int -> [a] -> [[a]]
recreate 0 = map (const [])
recreate j = hdsort . consCol . fork (id,recreate (j - 1))

rrot :: [a] -> [a]
rrot xs = last xs : init xs

-- | Sort a matrix on its first column
hdsort :: Ord a
       => [[a]] -> [[a]]
hdsort = sortBy cmp
  where cmp [] [] = EQ
        cmp [] _ = LT
        cmp _ [] = GT
        cmp (x:_) (y:_) = compare x y

consCol :: ([a],[[a]]) -> [[a]]
consCol = uncurry (zipWith (:))

fork :: (t2 -> t,t2 -> t1) -> t2 -> (t,t1)
fork (f,g) x = (f x,g x)

pair :: (t -> t1) -> (t,t) -> (t1,t1)
pair f (x,y) = (f x,f y)

untransform0 :: Ord a
             => ([a],Int) -> [a]
untransform0 (xs,k) = recreate (length xs) xs !! k

apply :: [Int] -> [t] -> [t]
apply p xs = [xs !! (p !! i)|i <- [0 .. length xs - 1]]

sortPerm :: (Enum b,Num b,Ord b,Ord a)
         => [a] -> [b]
sortPerm ys = snd <$> sort (zip ys [0 ..])

recreate2 :: (Ord a)
          => Int -> [a] -> [[a]]
recreate2 j ys = transpose . take j . tail . iterate (apply (sortPerm ys)) $ ys

untransform1 :: Ord a
             => ([a],Int) -> [a]
untransform1 (ys,k) =
  take (length ys)
       (tail $ (ys !!) <$> iterate (sortPerm ys !!) k)

untransform :: Ord a
            => ([a],Int) -> [a]
untransform (ys,k) = take n (tail $ (ya !) <$> iterate (pa !) k)
  where n = length ys
        ya = listArray (0,n - 1) ys
        pa =
          listArray (0,n - 1)
                    (snd <$> sort (zip ys [0 ..]))