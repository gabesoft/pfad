-- | Tests for Det
module Main (main) where

import Det
import Test.QuickCheck

data Matrix a =
  Matrix [[a]]
  deriving (Eq,Show)

instance Arbitrary a => Arbitrary (Matrix a) where
  arbitrary = sized f
    where f n =
            do xs <- vectorOf (m * m) arbitrary
               return (Matrix $ chunks m xs)
            where m = min n 30

main :: IO ()
main =
  do putStrLn "\nDetSpec"
     quickCheck (property prop_det)

prop_det :: Matrix Integer -> Bool
prop_det (Matrix xs) =
  det1 (fmap fromIntegral <$> xs) == fromIntegral (det3 xs)