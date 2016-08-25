-- | Solving the rush hour problem
module Rhp where

import qualified Data.Set as Set

-- | Breath first search (list implementation)
bfs
  :: Eq state
  => [state] -> [([move],state)] -> Maybe [move]
bfs _ [] = Nothing
bfs seen (p@(ms,q):ps)
  | solved q = Just ms
  | q `elem` seen = bfs seen ps
  | otherwise =
    bfs (q : seen)
        (ps ++ succs p)
  where succs (ms,q) = [(ms ++ [m],move q m)|m <- moves q]

solved = undefined

move = undefined

moves = undefined