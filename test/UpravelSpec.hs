-- | Tests for Upravel
module Main (main) where

import Upravel
import Test.QuickCheck

main :: IO ()
main =
  do putStrLn "\nUpravelSpec"
     quickCheck (property prop_supravel)

prop_supravel :: String -> Bool
prop_supravel s = supravel0 input == supravel input
  where input = take 10 s