-- | Tests for CxHull
module Main (main) where

import CxHull
import Test.QuickCheck

main :: IO ()
main =
  do putStrLn "\nCxHullSpec"
     quickCheck (prop_insideCH 3 10)

prop_insideCH :: Int -> Int -> Property
prop_insideCH d n =
  forAll (points d n) $
  \vs ->
    forAll (point d)
           (run vs)
  where run vs v = insideCH0 vs v == insideCH1 vs v

points :: Int -> Int -> Gen [[Integer]]
points d 0 = return []
points d n =
  do p <- point d
     ps <- points d (n - 1)
     return $ p : ps

point :: Int -> Gen [Integer]
point d = vector d >>= \xs -> return (xs ++ [1])