-- | Finding the shortest upravel
module Upravel where

import Data.List (minimumBy, sort)
import Data.Function (on)

supravel0 :: Ord a
          => [a] -> [[a]]
supravel0 = minimumBy (compare `on` length) . upravels

ascending :: Ord a
          => [a] -> Bool
ascending [] = True
ascending [_] = True
ascending (x:y:ys) = x <= y && ascending (y : ys)

unravels :: [t] -> [[[t]]]
unravels =
  foldr (concatMap . prefixes)
        [[]]

prefixes :: t -> [[t]] -> [[[t]]]
prefixes x [] = [[[x]]]
prefixes x (xs:xss) = ((x : xs) : xss) : map (xs :) (prefixes x xss)

upravels0 :: Ord a
          => [a] -> [[[a]]]
upravels0 = filter (all ascending) . unravels

upravels :: Ord a
         => [a] -> [[[a]]]
upravels =
  foldr (concatMap . uprefixes)
        [[]]

uprefixes :: Ord a
          => a -> [[a]] -> [[[a]]]
uprefixes x [] = [[[x]]]
uprefixes x (xs:xss) =
  if x <= head xs
     then ((x : xs) : xss) : map (xs :) (uprefixes x xss)
     else map (xs :) (uprefixes x xss)

insert :: Ord a
       => a -> [[a]] -> [[a]]
insert x [] = [[x]]
insert x (xs:xss) =
  if x <= head xs
     then (x : xs) : xss
     else xs : insert x xss

supravel :: Ord a
         => [a] -> [[a]]
supravel = foldr insert []