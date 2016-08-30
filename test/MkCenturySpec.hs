-- | Tests for MkCentury
module Main (main) where

import MkCentury
import Test.QuickCheck

main :: IO ()
main =
  do putStrLn "\nMkCenturySpec"
     quickCheck (property prop_solutions)

genNum :: Gen Int
genNum = choose (45,1000)

prop_solutions :: Property
prop_solutions =
  forAll genNum $
  \c ->
    solutions0 (fromIntegral c)
               digits ==
    solutions (fromIntegral c) digits
  where digits = [1 .. 9]
