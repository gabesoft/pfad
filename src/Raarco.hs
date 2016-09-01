-- | Rational arithmetic coding
module Raarco where

import Data.Maybe (fromJust)
import Data.Ratio

type Fraction = Ratio Integer

type Interval = (Fraction,Fraction)

type Symbol = Char

type Model = [(Symbol,Interval)]

narrow :: Interval -> Interval -> Interval
narrow (l1,r1) (l2,r2) = (l1 + (r1 - l1) * l2,l1 + (r1 - l1) * r2)

widen :: Fraction -> Interval -> Fraction
widen f (l,r) = (f - l) / (r - l)

within :: Fraction -> Interval -> Bool
within f (l,r) = l <= f && f < r

(|>) :: Interval -> Interval -> Interval
(|>) = narrow

(<|) :: Fraction -> Interval -> Fraction
(<|) = widen

-- | Return the interval associated with symbol x in model m
interval :: Model -> Symbol -> Interval
interval m x = fromJust (lookup x m)

-- | Return the symbol associated with the unique interval
-- | containing the proper fraction f
symbol :: Model -> Fraction -> Symbol
symbol m f = fst . head $ filter (within f . snd) m

adapt :: Model -> Symbol -> Model
adapt m _ = m

intervals :: Model -> [Symbol] -> [Interval]
intervals _ [] = []
intervals m (x:xs) = interval m x : intervals (adapt m x) xs

model :: Model
model = zip cs ys
  where xs = reverse $ (% 26) <$> [1 .. 26]
        ys = snd $ foldr f (0,[]) xs
        cs = ['a' .. 'z']
        f h (l,ts) = (h,(l,h) : ts)

encode1 :: Model -> [Symbol] -> Fraction
encode1 m = pick . foldl narrow (0,1) . intervals m

pick :: Interval -> Fraction
pick (l,r)
  | r <= (1 % 2) = pick (2 * l,2 * r) / 2
  | (1 % 2) <= l = (1 + pick (2 * l - 1,2 * r - 1)) / 2
  | otherwise = (1 % 2)

unfoldr :: (b -> Maybe (a,b)) -> b -> [a]
unfoldr f b =
  case f b of
    Just (a,c) -> a : unfoldr f c
    Nothing -> []

decode1 :: Model -> Fraction -> [Symbol]
decode1 m f = unfoldr decodeStep1 (m,(0,1),f)

decodeStep1
  :: (Model,Interval,Fraction) -> Maybe (Symbol,(Model,Interval,Fraction))
decodeStep1 (m,i,f) = Just (x,(adapt m x,i |> interval m x,f))
  where x = symbol m (f <| i)

encode2 :: Model -> [Symbol] -> [Int]
encode2 m = toBits . foldl narrow (0,1) . intervals m

decode2 :: Model -> [Int] -> [Symbol]
decode2 m bs = unfoldr decodeStep2 (m,(0,1),toFrac bs)

decodeStep2
  :: (Model,Interval,Fraction) -> Maybe (Symbol,(Model,Interval,Fraction))
decodeStep2 (m,i,f) = Just (x,(adapt m x,i |> interval m x,f))
  where x = symbol m (f <| i)

toBits :: Interval -> [Int]
toBits = unfoldr bit
  where bit :: Interval -> Maybe (Int,Interval)
        bit (l,r)
          | r <= (1 % 2) = Just (0,(2 * l,2 * r))
          | (1 % 2) <= l = Just (1,(2 * l - 1,2 * r - 1))
          | otherwise = Nothing

toFrac :: [Int] -> Fraction
toFrac bs =
  foldr (\b f -> (fromIntegral b + f) / 2)
        0
        (bs ++ [1])

stream
  :: (t -> Maybe (a,t)) -> (t -> t1 -> t) -> t -> [t1] -> [a]
stream producer consumer state as = unfoldr step (state,as)
  where step (s,xs) =
          case producer s of
            Just (y,t) -> Just (y,(t,xs))
            Nothing ->
              case xs of
                y:ys -> step (consumer s y,ys)
                [] -> Nothing