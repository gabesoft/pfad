-- | Tests for Minfree
module Main (main) where

import Data.List
import qualified Data.Set as Set
import qualified Data.Vector.Unboxed as U
import Data.Word
import Minfree
import Test.QuickCheck

main :: IO ()
main =
  do putStrLn "\nMinfreeSpec"
     quickCheck (property prop_altsort)
     quickCheck (property prop_minfree)
     quickCheck (property prop_minfreeST)
     quickCheck (property prop_minfree2)

prop_altsort :: [Word8] -> Bool
prop_altsort xs = U.toList (altsort ys) == sort ys
  where ys = fromIntegral <$> xs

prop_minfree :: [Word32] -> Bool
prop_minfree xs =
  minfree (fromIntegral <$> xs) == fromIntegral (expectedMinfree xs)

prop_minfree2 :: Set.Set Word32 -> Bool
prop_minfree2 xs =
  minfree2 (fromIntegral <$> ys) == fromIntegral (expectedMinfree ys)
  where ys = Set.toList xs

prop_minfreeST :: [Word32] -> Bool
prop_minfreeST xs =
  minfreeST (fromIntegral <$> xs) == fromIntegral (expectedMinfree xs)

expectedMinfree :: (Eq a,Num a,Enum a)
                => [a] -> a
expectedMinfree = head . except [0 ..]
  where except us vs = filter (`notElem` vs) us
