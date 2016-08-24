-- | Tests for BoyerMoore
module Main (main) where

import BoyerMoore
import Data.Word
import Test.QuickCheck

main :: IO ()
main =
  do putStrLn "\nBoyerMooreSpec"
     quickCheck (property prop_matches)

prop_matches :: Word8 -> String -> Bool
prop_matches n xs = matches0 xs ys == matches xs ys
  where ys = (concat . replicate (max 2 (fromIntegral n))) xs
