-- | Tests for Kmp
module Main (main) where

import Kmp
import Data.Word
import Test.QuickCheck

main :: IO ()
main =
  do putStrLn "\nKmpSpec"
     quickCheck (property prop_matches)

prop_matches :: Word8 -> String -> Bool
prop_matches n xs = matches1 xs ys == matches xs ys
  where ys = (concat . replicate (max 3 (fromIntegral n))) xs
