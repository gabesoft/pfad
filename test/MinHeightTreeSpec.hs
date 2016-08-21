-- | Tests for MinHeightTree
module Main (main) where

import Data.Word
import MinHeightTree
import Test.QuickCheck

main :: IO ()
main =
  do putStrLn "\nMinHeightTreeSpec"
     quickCheck (property prop_mincost)

prop_mincost :: Word16 -> Bool
prop_mincost n = cost (mincostTree [1 .. (n + 1)]) <= n + 2