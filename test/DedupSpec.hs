-- | Tests for Dedup
module Main (main) where

import Data.List
import Dedup
import Test.QuickCheck

main :: IO ()
main =
  do putStrLn "\nDedupSpec"
     quickCheck (property prop_dedup)

prop_dedup :: [Integer] -> Bool
prop_dedup xs = nub xs == dedup xs