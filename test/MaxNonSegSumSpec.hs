-- | Tests for MaxNonSegSum
module Main (main) where

import MaxNonSegSum
import Test.QuickCheck

main :: IO ()
main =
  do putStrLn "\nMaxNonSegSumSpec"
     quickCheck (property prop_mnss)

prop_mnss
  :: Integer -> Integer -> Integer -> [Integer] -> Bool
prop_mnss a b c xs = mnss0 ys == mnss ys
  where ys = take 12 (a : b : c : xs)