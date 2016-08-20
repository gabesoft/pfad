-- | Tests for Saddleback
module Main (main) where

import Control.Monad
import Data.List (sort)
import Data.Word
import Saddleback
import Test.QuickCheck

main :: IO ()
main =
  do putStrLn "\nSaddlebackSpec"
     -- using other types than Integer in the tests below will result in test failures
     -- due to overflow
     -- see f0(0 :: Int,64 :: Int) vs f0(0 :: Integer,64 :: Integer)
     forM_ [f0,f1,f2,f3,f4] run
  where run :: ((Integer,Integer) -> Integer) -> IO ()
        run f = quickCheck (property $ prop_invert f)

prop_invert :: (Integral a,Num a)
            => ((a,a) -> a) -> Word8 -> Bool
prop_invert f z =
  (sort . invert0 f) (fromIntegral z) == (sort . invert f) (fromIntegral z)

f0 :: (Integral b,Num a)
   => (a,b) -> a
f0 (x,y) = (2 ^ y) * (2 * x + 1) - 1

f1 :: Integral b
   => (b,b) -> b
f1 (x,y) = x * (2 ^ x) + y * (2 ^ y) + 2 * x + y

f2 :: Num a
   => (a,a) -> a
f2 (x,y) = 3 * x + 27 * y + y * y

f3 :: Num a
   => (a,a) -> a
f3 (x,y) = x * x + y * y + x + y

f4 :: Integral a
   => (a,a) -> a
f4 (x,y) = x + (2 ^ y) + y - 1