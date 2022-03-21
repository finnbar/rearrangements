module Rearrange.Rearrangeable where

import Data.HList
import Data.Kind

class Rearrangeable (t :: [k] -> *) where
    rConsToHead :: t (x ': xs) -> (t xs' -> t (x ': xs'))
    rTail :: t (x ': xs) -> t xs
    rEmpty :: t '[]

class RearrangeableStar (t :: [*] -> *) where
    rCons :: t xs -> t ys -> t (t xs ': ys)
    rHead :: t (t x ': xs) -> t x

instance Rearrangeable HList where
    rConsToHead (x :+: xs) = (x :+:)
    rTail (_ :+: xs) = xs
    rEmpty = HNil

instance RearrangeableStar HList where
    rCons = (:+:)
    rHead (x :+: _) = x