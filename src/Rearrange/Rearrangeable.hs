module Rearrange.Rearrangeable where

import Data.HList

class Rearrangeable t where
    rHead :: t (x ': xs) -> x
    rTail :: t (x ': xs) -> t xs
    rCons :: x -> t xs -> t (x ': xs)
    rEmpty :: t '[]

instance Rearrangeable HList where
    rHead (x :+: xs) = x
    rTail (x :+: xs) = xs
    rCons = (:+:)
    rEmpty = HNil