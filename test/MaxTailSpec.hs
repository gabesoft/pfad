-- | Tests for MaxTail
module Main (main) where

import MaxTail
import Test.QuickCheck

main :: IO ()
main =
  do putStrLn "\nMaxTailSpec"
     quickCheck (property prop_maxtail)

prop_maxtail :: Char -> String -> Bool
prop_maxtail x xs = maxtail str == maxtail1 str
  where str = x : xs
