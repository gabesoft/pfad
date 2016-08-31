-- | Rational arithmetic coding
module Raarco where

import Data.Ratio

type Fraction = Ratio Integer

type Interval = (Fraction,Fraction)

narrow :: Interval -> Interval -> Interval
narrow (l1,r1) (l2,r2) = (l1 + (r1 - l1) * l2,l1 + (r1 - l1) * r2)

widen :: Fraction -> Interval -> Fraction
widen f (l,r) = (f - l) / (r - l)
