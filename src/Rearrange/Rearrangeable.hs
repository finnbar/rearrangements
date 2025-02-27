module Rearrange.Rearrangeable where

import Data.HList
import Data.Kind

-- A pair of type classes that allow rearrangements to be used on them.

-- Rearrangeable allows for structures to be rearranged, e.g.
-- HList '[Bool, Int, ()] can be rearranged to HList '[Int, (), Bool].
-- This is necessary since these structures can contain types of arbitrary kind
-- k, so we can't just take the head of a structure easily (since we can only
-- return a value of type of kind Type).
class Rearrangeable (t :: [k] -> Type) where
    rConsToHead :: t (x ': xs) -> (t xs' -> t (x ': xs'))
    rTail :: t (x ': xs) -> t xs
    rEmpty :: t '[]

-- RearrangeableStar also allows for nested structures to be rearranged, since
-- the fully applied containers themselves are of kind Type. e.g.
-- HList '[HList '[Bool, Int], HList '[()]] can be rearranged to
-- HList '[HList '[Int], HList '[(), Bool]].
class RearrangeableStar (t :: [Type] -> Type) where
    rCons :: t xs -> t ys -> t (t xs ': ys)
    rHead :: t (t x ': xs) -> t x

instance Rearrangeable HList where
    rConsToHead (x :+: xs) = (x :+:)
    rTail (_ :+: xs) = xs
    rEmpty = HNil

instance RearrangeableStar HList where
    rCons = (:+:)
    rHead (x :+: _) = x