-- | Finding the smallest free natural number not in a set
module Minfree (minfree, minfreeST, minfree2, checklist, countlist, altsort)
       where

import qualified Data.Vector as V
import Data.Vector.Unboxed.Mutable (new, write)
import qualified Data.Vector.Unboxed as U

-- | Find the smallest free natural number not in the input list
minfree :: [Int] -> Int
minfree = search . checklist

-- | Find the smallest free natural number not in the input list
-- | This solution uses a stateful implementation
minfreeST :: [Int] -> Int
minfreeST = search . checklistST

search :: U.Vector Bool -> Int
search = U.length . U.takeWhile id

checklist :: [Int] -> U.Vector Bool
checklist xs = v U.// bs
  where n = length xs
        v =
          U.replicate (n + 1)
                      False
        bs =
          zip (filter (<= n) xs)
              (repeat True)

checklistST :: [Int] -> U.Vector Bool
checklistST xs =
  U.create $
  do v <- new (n + 1)
     U.forM_ (U.fromList $ filter (<= n) xs)
             (\x -> write v x True)
     return v
  where n = length xs

countlist :: [Int] -> U.Vector Int
countlist xs =
  U.accum (+)
          (U.replicate (maximum xs + 1)
                       0)
          (zip xs (repeat 1))

-- | Sorting in O(maximum xs)
--   Only suitable for lists containing low numbers
altsort :: [Int] -> U.Vector Int
altsort [] = U.empty
altsort xs = V.foldr (U.++) U.empty vs
  where vs =
          V.imap (flip U.replicate)
                 (V.convert $ countlist xs)

-- | Find the smallest free natural number not in the input list
-- | The input list must not contain duplicate entries
minfree2 :: [Int] -> Int
minfree2 xs = minfrom 0 (U.length ys,ys)
  where ys = U.fromList xs

minfrom :: Int -> (Int, U.Vector Int) -> Int
minfrom a (n,xs)
  | n == 0 = a
  | m == b - a = minfrom b (n - m,vs)
  | otherwise = minfrom a (m,us)
  where (us,vs) = U.partition (< b) xs
        b = a + 1 + n `div` 2
        m = U.length us
