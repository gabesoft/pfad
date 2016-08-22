-- | Tests for Bwt
module Main (main) where

import Bwt
import Test.QuickCheck

main :: IO ()
main =
  do putStrLn "\nBwtSpec"
     quickCheck (property prop_transform)

prop_transform :: Char -> String -> Bool
prop_transform x xs = (untransform . transform) str == str
  where str = x : xs