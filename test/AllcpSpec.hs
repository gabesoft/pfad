-- | Tests for Allcp
module Main (main) where

import Allcp
import Data.Word
import Test.QuickCheck

main :: IO ()
main =
  do putStrLn "\nAllcpSpec"
     quickCheck (property prop_allcp)
     quickCheck (property prop_allcp2)

prop_allcp :: [Word8] -> Bool
prop_allcp xs = allcp0 xs == allcp xs

prop_allcp2 :: [Word8] -> Bool
prop_allcp2 xs = allcp0 (xs ++ xs ++ xs) == allcp (xs ++ xs ++ xs)