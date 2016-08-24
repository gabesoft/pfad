-- | The Boyer-Moore algorithm
module BoyerMoore where

import Data.List (inits)

matches0 :: Eq a
         => [a] -> [a] -> [Int]
matches0 ws = map length . filter (endswith ws) . inits

matches1 ws = map fst . filter snd . map (fork (length,endswith ws)) . inits

endswith = undefined

fork (f,g) x = (f x,g x)