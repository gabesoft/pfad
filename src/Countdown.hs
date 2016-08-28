-- | Countdown solutions
module Countdown where

data Expr
  = Num Int
  | App Op
        Expr
        Expr
  deriving (Eq)

data Op
  = Add
  | Sub
  | Mul
  | Div
  deriving (Eq,Show)

type Value = Int

data Trie a =
  Node a
       [(Int,Trie a)]
  deriving (Eq,Show)

type Memo = Trie [(Expr,Value)]

instance Show Expr where
  show = printExpr

data Tree = Tip Int | Bin Tree Tree deriving (Eq, Show)

type MemoT = Trie [Tree]

sample1 :: (Int,[Int])
sample1 = (831,[1,3,7,10,25,50])

sample2 :: (Int,[Int])
sample2 = (103117,[1,3,7,10,25,50,13,71])

subseqs :: [t] -> [[t]]
subseqs [] = []
subseqs [x] = [[x]]
subseqs (x:xs) = xss ++ [x] : map (x :) xss
  where xss = subseqs xs

value :: Expr -> Value
value (Num x) = x
value (App op e1 e2) =
  apply op
        (value e1)
        (value e2)

apply :: Integral a
      => Op -> a -> a -> a
apply Add = (+)
apply Sub = (-)
apply Mul = (*)
apply Div = div

legal :: Op -> Value -> Value -> Bool
legal Add v1 v2 = v1 <= v2
legal Sub v1 v2 = v1 >= v2
legal Mul v1 v2 = 1 < v1 && v1 <= v2
legal Div v1 v2 = 1 < v2 && v1 `mod` v2 == 0

mkExprs :: [Int] -> [(Expr,Value)]
mkExprs [x] = [(Num x,x)]
mkExprs xs =
  [ev
  |(ys,zs) <- unmerges xs
  ,ev1 <- mkExprs ys
  ,ev2 <- mkExprs zs
  ,ev <- combine ev1 ev2]

unmerges :: [t] -> [([t],[t])]
unmerges [x,y] = [([x],[y])]
unmerges (x:xs) =
  ([x],xs) :
  concatMap (add x)
            (unmerges xs)
  where add k (ys,zs) = [(k : ys,zs),(ys,k : zs)]

combine
  :: (Expr,Value) -> (Expr,Value) -> [(Expr,Value)]
combine (e1,v1) (e2,v2) =
  [(App op e1 e2,apply op v1 v2)|op <- ops,legal op v1 v2] ++
  [(App op e2 e1,apply op v2 v1)|op <- ops,legal op v2 v1]
  where ops = [Add,Sub,Mul,Div]

nearest :: (Num t1,Ord t1)
        => t1 -> [(t,t1)] -> (t,t1)
nearest n ((e,v):evs)
  | d == 0 = (e,v)
  | otherwise = search n d (e,v) evs
  where d = abs (n - v)

search :: (Num a,Ord a)
       => a -> a -> (t,a) -> [(t,a)] -> (t,a)
search n d ev [] = ev
search n d ev ((e,v):evs)
  | d' == 0 = (e,v)
  | d' < d = search n d' (e,v) evs
  | d' >= d = search n d ev evs
  where d' = abs (n - v)

countdown1 :: Value -> [Int] -> (Expr,Value)
countdown1 n = nearest n . concatMap mkExprs . subseqs

display :: (Expr,Value) -> String
display (e,v) = printExpr e ++ " = " ++ show v

printExpr :: Expr -> String
printExpr (Num x) = show x
printExpr (App op e1 e2) = "(" ++ printExpr e1 ++ printOp op ++ printExpr e2 ++ ")"

printOp :: Op -> [Char]
printOp Add = " + "
printOp Sub = " - "
printOp Mul = " * "
printOp Div = " / "

comb1 :: Integral a
      => (Expr,a) -> (Expr,a) -> [(Expr,a)]
comb1 (e1,v1) (e2,v2) =
  (if non Sub e1 && non Sub e2
      then [(App Add e1 e2,v1 + v2)|non Add e2] ++ [(App Sub e2 e1,v2 - v1)]
      else []) ++
  (if 1 < v1 && non Div e1 && non Div e2
      then [(App Mul e1 e2,v1 * v2)|non Mul e2] ++ [(App Div e2 e1,q)|r == 0]
      else [])
  where (q,r) = divMod v2 v1

comb2 :: (Num a,Ord a)
      => (Expr,a) -> (Expr,a) -> [(Expr,a)]
comb2 (e1,v1) (e2,v2) =
  [(App Add e1 e2,v1 + v2)|non Sub e1,non Add e2,non Sub e2] ++
  (if 1 < v1 && non Div e1 && non Div e2
      then [(App Mul e1 e2,v1 * v2)|non Mul e2] ++ [(App Div e1 e2,1)]
      else [])

combine2 :: Integral a
         => (Expr,a) -> (Expr,a) -> [(Expr,a)]
combine2 (e1,v1) (e2,v2)
  | v1 < v2 =
    comb1 (e1,v1)
          (e2,v2)
  | v1 == v2 =
    comb2 (e1,v1)
          (e2,v2)
  | v1 > v2 =
    comb1 (e2,v2)
          (e1,v1)

countdown2 :: Value -> [Int] -> (Expr,Value)
countdown2 n = nearest n . concatMap mkExprs2 . subseqs

mkExprs2 :: [Int] -> [(Expr,Value)]
mkExprs2 [x] = [(Num x,x)]
mkExprs2 xs =
  [ev
  |(ys,zs) <- unmerges xs
  ,ev1 <- mkExprs2 ys
  ,ev2 <- mkExprs2 zs
  ,ev <- combine2 ev1 ev2]

non :: Op -> Expr -> Bool
non _ (Num _) = True
non op1 (App op2 _ _) = op1 /= op2

legal2
  :: Op -> (Expr,Value) -> (Expr,Value) -> Bool
legal2 Add (e1,v1) (e2,v2) =
  (v1 <= v2) && non Sub e1 && non Add e2 && non Sub e2
legal2 Sub (e1,v1) (e2,v2) = (v2 < v1) && non Sub e1 && non Sub e2
legal2 Mul (e1,v1) (e2,v2) =
  (1 < v1 && v1 <= v2) && non Div e1 && non Mul e2 && non Div e2
legal2 Div (e1,v1) (e2,v2) =
  (1 < v2 && v1 `mod` v2 == 0) && non Div e1 && non Div e2

mkExprs3 :: Memo -> [Int] -> [(Expr,Value)]
mkExprs3 _ [x] = [(Num x,x)]
mkExprs3 memo xs =
  [ev
  |(ys,zs) <- unmerges xs
  ,ev1 <- fetch memo ys
  ,ev2 <- fetch memo zs
  ,ev  <- combine2 ev1 ev2]

memoise :: [[Int]] -> Memo
memoise = foldl insert empty
  where insert memo xs = store xs (mkExprs3 memo xs) memo

empty :: Memo
empty = Node [] []

fetch :: Trie a -> [Int] -> a
fetch (Node es _) [] = es
fetch (Node _ xms) (x:xs) = fetch (follow x xms) xs
  where follow y yms = head [m | (y',m) <- yms,y == y']

store :: [Int] -> a -> Trie a -> Trie a
store [x] es (Node fs xms) = Node fs ((x,Node es []) : xms)
store (x:xs) es (Node fs xms) = Node fs (yms ++ (x,store xs es m) : zms)
  where (yms,(_,m):zms) = break (equals x) xms
        equals y (z,_) = y == z

extract :: Memo -> [(Expr,Value)]
extract (Node es xms) = es ++ concatMap (extract . snd) xms

mkTrees :: MemoT -> [Int] -> [Tree]
mkTrees _ [x] = [Tip x]
mkTrees memo xs = [Bin t1 t2|(ys,zs) <- unmerges xs,t1 <- fetch memo ys,t2 <- fetch memo zs]

toExprs :: Tree -> [(Expr,Value)]
toExprs (Tip x) = [(Num x,x)]
toExprs (Bin t1 t2) = [ev | ev1 <- toExprs t1
                          , ev2 <- toExprs t2
                          , ev  <- combine ev1 ev2]

-- display $ uncurry countdown1 sample1
-- display $ uncurry countdown4 sample2
countdown4 :: Int -> [Int] -> (Expr,Value)
countdown4 n = nearest n . extract . memoise . subseqs

countdown5 :: Int -> [Int] -> (Expr,Value)
countdown5 n = nearest n . concatMap toExprs . extract2 . memoise2 . subseqs

extract2 :: MemoT -> [Tree]
extract2 (Node es xms) = es ++ concatMap (extract2 . snd) xms

memoise2 :: [[Int]] -> MemoT
memoise2 = foldl insert (Node [] [])
  where insert memo xs = store xs (mkTrees memo xs) memo
