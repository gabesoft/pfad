-- | Listing all ways the operations + and x can be inserted
--   into the list of digits [1..9] to amount to a total of 100
module MkCentury where

import Data.List

type Expression = [Term]

type Term = [Factor]

type Factor = [Digit]

type Digit = Int

fork :: (t2 -> t,t2 -> t1) -> t2 -> (t,t1)
fork (f,g) x = (f x,g x)

cross :: (t -> t2,t1 -> t3) -> (t,t1) -> (t2,t3)
cross (f,g) (x,y) = (f x,g y)

uzip :: ([a],[b]) -> [(a,b)]
uzip = uncurry zip

good100 :: (Eq a,Num a)
        => a -> Bool
good100 v = v == 100

valFact :: Factor -> Int
valFact = foldl (\n d -> 10 * n + d) 0

valTerm :: Term -> Int
valTerm = product . map valFact

valExpr :: Expression -> Int
valExpr = sum . map valTerm

glue0 :: Digit -> Expression -> [Expression]
glue0 x ((xs:xss):xsss) =
  [((x : xs) : xss) : xsss,([x] : xs : xss) : xsss,[[x]] : (xs : xss) : xsss]

extend0 :: Digit -> [Expression] -> [Expression]
extend0 x [] = [[[[x]]]]
extend0 x es = concatMap (glue0 x) es

expressions :: [Digit] -> [Expression]
expressions = foldr extend0 []

-- forM_ (showExpr <$> solutions0 100 [1..9]) putStrLn
-- forM_ (showExpr<$>solutions0 121 [1..9]) putStrLn
-- forM_ (showExpr<$>solutions0 200 [1..9]) putStrLn
-- forM_ (showExpr<$>solutions0 2001 ([1..9]++[5..9])) putStrLn
solutions0 :: Int -> [Digit] -> [Expression]
solutions0 c = filter ((== c) . valExpr) . expressions

showExpr :: Expression -> String
showExpr terms =
  intercalate " + "
              (showTerm <$> terms)

showTerm :: Term -> String
showTerm factors =
  intercalate " x "
              (showFactor <$> factors)

showFactor :: Factor -> String
showFactor = show . valFact

value :: Expression -> (Integer,Int,Int,Int)
value ((xs:xss):xsss) = (10 ^ n,valFact xs,valTerm xss,valExpr xsss)
  where n = length xs

modify :: Num t
       => t -> (t,t,t,t) -> [(t,t,t,t)]
modify x (k,f,t,e) = [(10 * k,k * x + f,t,e),(10,x,f * t,e),(10,x,1,f * t + e)]

good :: (Eq a,Num a)
     => a -> (t,a,a,a) -> Bool
good c (_,f,t,e) = (f * t + e == c)

ok :: (Num a,Ord a)
   => a -> (t,a,a,a) -> Bool
ok c (_,f,t,e) = f * t + e <= c

-- forM_ (showExpr<$>solutions 2013 ([1..9]++[5..9])) putStrLn
solutions :: (Num a,Ord a,Foldable t)
          => a -> t a -> [[[[a]]]]
solutions c = map fst . filter (good c . snd) . foldr (expand c) []

expand
  :: (Num t,Ord t)
  => t -> t -> [([[[t]]],(t,t,t,t))] -> [([[[t]]],(t,t,t,t))]
expand c x [] = [([[[x]]],(10,x,1,0))]
expand c x evs = concat (map (filter (ok c . snd) . glue x) evs)

glue
  :: Num t
  => t -> ([[[t]]],(t,t,t,t)) -> [([[[t]]],(t,t,t,t))]
glue x ((xs:xss):xsss,(k,f,t,e)) =
  [(((x : xs) : xss) : xsss,(10 * k,k * x + f,t,e))
  ,(([x] : xs : xss) : xsss,(10,x,f * t,e))
  ,([[x]] : (xs : xss) : xsss,(10,x,1,f * t + e))]