-- | Hylomorphisms
module Hylo where

import qualified Data.Tree as T

data Tree a
  = Leaf a
  | Node [Tree a]
  deriving (Eq,Show)

instance Functor Tree where
  fmap f (Leaf a) = Leaf (f a)
  fmap f (Node t) = Node (fmap f <$> t)

instance Applicative Tree where
  pure = Leaf
  Leaf f <*> Leaf x = Leaf (f x)
  Leaf f <*> Node t = Node (fmap f <$> t)
  Node tfs <*> Node txs = Node (fmap (<*> Node txs) tfs)
  Node tfs <*> Leaf x = Node tfs <*> Node [Leaf x]

display :: Show a
        => Tree a -> String
display t = T.drawTree (fold1 toTree t)
  where toTree (Left x) = T.Node (show x) []
        toTree (Right xs) = T.Node "◯" xs

printTree :: Show a
          => Tree a -> IO ()
printTree = putStrLn . display

hylo1
  :: Functor f
  => (Either a (f b) -> b) -> (c -> Either a (f c)) -> c -> b
hylo1 f g x =
  case g x of
    Left y -> f (Left y)
    Right xs -> f (Right (hylo1 f g <$> xs))

fold :: (a -> b) -> ([b] -> b) -> Tree a -> b
fold f _ (Leaf x) = f x
fold f g (Node t) = g (fold f g <$> t)

fold1 :: (Either a [b] -> b) -> Tree a -> b
fold1 f =
  fold (f . Left)
       (f . Right)

-- unfold (> 3) (+1) (\x -> [x + 1, x + 2]) 0
unfold
  :: (b -> Bool) -> (b -> a) -> (b -> [b]) -> b -> Tree a
unfold p v h x =
  if p x
     then Leaf (v x)
     else Node (unfold p v h <$> h x)

-- unfold1 (\x -> if x > 3 then Left (x + 1) else Right [x+1,x+2]) 0
unfold1 :: (b -> Either a [b]) -> b -> Tree a
unfold1 g x =
  case g x of
    Left y -> Leaf y
    Right xs -> Node (unfold1 g <$> xs)

-- hylo2 = fold f g . unfold p v h
hylo2
  :: Functor f
  => (a -> b) -> (f b -> b) -> (a -> Bool) -> t -> (a -> f a) -> a -> b
hylo2 f g p v h x =
  if p x
     then f x
     else g (hylo2 f g p v h <$> h x)

sumTree :: Num a
        => Tree a -> a
sumTree = fold id sum

-- putStrLn . T.drawTree $ fill show show (unfold (> 3) (+1) (\x -> [x + 1, x + 2]) 0)
-- putStrLn . T.drawTree $ show <$> fill id sum (unfold (> 3) (+1) (\x -> [x + 1, x + 2]) 0)
-- putStrLn . T.drawTree $ fill id concat (mkTree splitMid "abcde")
-- putStrLn . T.drawTree $ fill id concat (mkTree isegs "abcde")
fill
  :: (a -> b) -> ([b] -> b) -> Tree a -> T.Tree b
fill f g =
  fold (lleaf f)
       (lnode g)

lleaf :: (t -> a) -> t -> T.Tree a
lleaf f x = T.Node (f x) []

lnode :: ([a] -> a) -> [T.Tree a] -> T.Tree a
lnode g t = T.Node (g $ label <$> t) t

label :: T.Tree t -> t
label (T.Node x _) = x

-- hylo id mergeFold isegs [1..4]
hylo
  :: ([t] -> c) -> ([c] -> c) -> ([t] -> [[t]]) -> [t] -> c
hylo f g h = fold f g . mkTree h

-- (show . sum) <$> mkTree splitMid [1..4]
mkTree :: ([t] -> [[t]]) -> [t] -> Tree [t]
mkTree = unfold single id

single :: [t] -> Bool
single [_] = True
single _ = False

-- printTree $ mkTree splitMid "abcdef"
splitMid :: [a] -> [[a]]
splitMid xs = [ws,zs]
  where (ws,zs) = splitAt (length xs `div` 2) xs

-- | Merge a list of sorted lists into a sorted list
-- | mergeFold [[3..5],[1..5],[0..9]]
mergeFold :: (Foldable t,Ord b)
          => t [b] -> [b]
mergeFold = foldr merge []
  where merge :: Ord a
              => [a] -> [a] -> [a]
        merge [] ys = ys
        merge xs [] = xs
        merge (x:xs) (y:ys)
          | x <= y = x : merge xs (y : ys)
          | otherwise = y : merge (x : xs) ys

-- | Sort a list using merge sort
-- | mergeSort "zwakbxcyd"
mergeSort :: (Ord a)
          => [a] -> [a]
mergeSort [] = []
mergeSort xs = hylo id mergeFold splitMid xs

-- putStrLn . T.drawTree $ fill id recover (mkTree isegs "abcde")
recover :: [[a]] -> [a]
recover xss = head (head xss) : last xss

-- | Return the sub-sequences of the input list in which
-- | just one argument is dropped
minors :: [t] -> [[t]]
minors [] = []
minors [x,y] = [[x],[y]]
minors (x:xs) = map (x :) (minors xs) ++ [xs]

uncats :: [t] -> [([t],[t])]
uncats [] = []
uncats [x,y] = [([x],[y])]
uncats (x:xs) =
  ([x],xs) :
  map (cons x)
      (uncats xs)
  where cons h (ys,zs) = (h : ys,zs)

-- | Return a list composed of the two immediate segments of a list
-- | isegs "abcde" = ["abcd","bcde"]
isegs :: [a] -> [[a]]
isegs [] = []
isegs xs = [init xs,tail xs]

-- hylo id (map sum) isegs [1..10]
-- solveIsegs id (map sum) [1..10]
-- solveIsegs id (map sum) [1..1000] -- this will time out as a hylo
solveIsegs
  :: ([a] -> b) -> ([b] -> b) -> [a] -> b
solveIsegs f g = head . until single (map g . group) . map (f . wrap)
  where group :: [t] -> [[t]]
        group []       = []
        group [_]      = []
        group (x:y:xs) = [x,y] : group (y : xs)
        wrap           = (: [])