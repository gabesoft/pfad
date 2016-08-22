-- | Tests for RankTails
module Main (main) where

import RankTails
import Test.QuickCheck

main :: IO ()
main =
  do putStrLn "\nRankTailsSpec"
     quickCheck (property prop_rank)

prop_rank :: [Integer] -> Bool
prop_rank xs = rank0 xs == rank xs