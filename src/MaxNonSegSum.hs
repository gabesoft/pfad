-- | Calculating the maximum non-segment sum
module MaxNonSegSum where

data NSState
  = E
  | S
  | M
  | N
  deriving (Eq,Show,Ord)

sample :: [Int]
sample = [-4,-3,-7,2,1,-2,-1,-4]

markings :: [a] -> [[(a,Bool)]]
markings xs = [zip xs bs|bs <- booleans (length xs)]

booleans :: (Eq a,Num a)
         => a -> [[Bool]]
booleans 0 = [[]]
booleans n = [b : bs|b <- [True,False],bs <- booleans (n - 1)]

nonsegs :: [a] -> [[a]]
nonsegs = extract . filter nonseg . markings

extract :: [[(a,Bool)]] -> [[a]]
extract = map (map fst . filter snd)

nonseg :: [(a,Bool)] -> Bool
nonseg = (== N) . foldl sstep E . map snd

-- | Compute the maximum non-segment sum
mnss0 :: (Ord a,Num a)
      => [a] -> a
mnss0 = maximum . map sum . nonsegs

sstep :: NSState -> Bool -> NSState
sstep E False = E
sstep E True = S
sstep S False = M
sstep S True = S
sstep M False = M
sstep M True = N
sstep N _ = N

pick :: NSState -> [a] -> [[a]]
pick st = extract . filter ((== st) . foldl sstep E . map snd) . markings

nonsegs1 :: [a] -> [[a]]
nonsegs1 = pick N

pickall :: [a] -> ([[a]],[[a]],[[a]],[[a]])
pickall = foldl step ([[]],[],[],[])
  where step (ess,nss,mss,sss) x =
          (ess
          ,map (++ [x])
               (sss ++ ess)
          ,mss ++ sss
          ,nss ++
           map (++ [x])
               (nss ++ mss))

mnss1 :: (Ord a,Num a)
      => [a] -> a
mnss1 = maximum . map sum . fourth . pickall

mnss2 :: (Ord a,Num a)
      => [a] -> a
mnss2 = fourth . mapTuple4 (maximum . map sum) . pickall

fourth :: (t,t1,t2,t3) -> t3
fourth (_,_,_,d) = d

mapTuple4
  :: (t -> t1) -> (t,t,t,t) -> (t1,t1,t1,t1)
mapTuple4 f (a,b,c,d) = (f a,f b,f c,f d)

-- | Compute the maximum non-segment sum in O(n)
-- | The input list must contain at least 3 elements
mnss :: (Ord a,Num a)
     => [a] -> a
mnss xs =
  fourth $
  foldl h
        (start $ take 3 xs)
        (drop 3 xs)
  where h (e,s,m,n) x = (e,(s `max` e) + x,m `max` s,n `max` ((n `max` m) + x))
        start [x,y,z] =
          (0,maximum [x + y + z,y + z,z],maximum [x,x + y,y],x + z)