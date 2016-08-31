-- | Sudoku solver
module Sudoku where

import Data.List ((\\))

type Row a = [a]

type Matrix a = [Row a]

type Digit = Char

type Grid = Matrix Digit

type Choices = [Digit]

sample :: Grid
sample = ["004005700"
         ,"000009400"
         ,"360000008"
         ,"720060000"
         ,"000402000"
         ,"000080093"
         ,"400000056"
         ,"005300000"
         ,"006100900"]

digits :: [Char]
digits = ['1' .. '9']

blank :: Char -> Bool
blank = (== '0')

choices :: Grid -> Matrix Choices
choices = map (map choice)

choice :: Char -> [Char]
choice d =
  if blank d
     then digits
     else [d]

expand :: Matrix Choices -> [Grid]
expand = cp . map cp

cp :: [[t]] -> [[t]]
cp [] = [[]]
cp (xs:xss) = [x : ys|x <- xs,ys <- cp xss]

valid :: Grid -> Bool
valid g = all nodups (rows g) && all nodups (cols g) && all nodups (boxs g)

nodups :: Eq a
       => [a] -> Bool
nodups [] = True
nodups (x:xs) = x `notElem` xs && nodups xs

rows :: Matrix a -> Matrix a
rows = id

cols :: Matrix a -> Matrix a
cols [xs] = [[x]|x <- xs]
cols (xs:xss) = zipWith (:) xs (cols xss)

transpose :: [[a]] -> [[a]]
transpose = cols

boxs :: Matrix a -> Matrix a
boxs = boxsN 3

boxsN :: Int -> [[a]] -> [[a]]
boxsN n = map ungroup . ungroup . map cols . group n . map (group n)

group :: Int -> [a] -> [[a]]
group _ [] = []
group n xs = take n xs : group n (drop n xs)

ungroup :: [[a]] -> [a]
ungroup = concat

pruneRow :: Row Choices -> Row Choices
pruneRow row = map (remove fixed) row
  where fixed = [d | [d] <- row]

remove :: Eq t => [t] -> [t] -> [t]
remove _ [] = []
remove _ [d] = [d]
remove xs ds = ds \\ xs

pruneBy :: ([Row Choices] -> [Row Choices]) -> [Row Choices] -> [Row Choices]
pruneBy f = f . map pruneRow . f

prune :: [Row Choices] -> [Row Choices]
prune = pruneBy boxs . pruneBy cols . pruneBy rows

-- solve = filter valid . expand . prune . choices
-- mapM_ (putStrLn . intersperse ' ') (head (solve sample))
solve :: Grid -> [Grid]
solve = search . choices
  where search m
          | not (safe m) = []
          | complete m' = [map (map head) m']
          | otherwise = concatMap search (expand1 m')
          where m' = prune m

complete :: [[[t]]] -> Bool
complete = all (all single)
  where single [x] = True
        single _ = False

safe :: Eq a => Matrix [a] -> Bool
safe m = all ok (rows m) && all ok (cols m) && all ok (boxs m)
  where ok row = nodups [d | [d] <- row]

expand1 :: Matrix Choices -> [Matrix Choices]
expand1 rows = [rows1 ++ [row1 ++ [c] : row2] ++ rows2|c <- cs]
  where (rows1,row:rows2) = break (any smallest) rows
        (row1,cs:row2) = break smallest row
        smallest xs = length xs == minimum (counts rows)
        counts = filter (/= 1) . map length . concat
