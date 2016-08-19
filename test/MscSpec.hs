-- | Tests for Msc
module Main (main) where

import Msc
import Test.QuickCheck

main :: IO ()
main =
  do putStrLn "\nMscSpec"
     quickCheck (property prop_msc)

prop_msc :: [Int] -> Bool
prop_msc xs = msc xs == msc0 xs