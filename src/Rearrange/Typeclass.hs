{-# LANGUAGE UndecidableInstances, ScopedTypeVariables,
    FlexibleContexts, AllowAmbiguousTypes #-}

module Rearrange.Typeclass where

import Rearrange.Rearrangeable
import Rearrange.TypeFamilies

-- **********
-- Rearrange arbitrary HLists into other HLists (no deletion)
-- **********

class Rearrangeable t => Rearrange t as bs where
    rearr :: t as -> t bs

-- Base case: result is empty.
instance Rearrangeable t => Rearrange t as '[] where
    rearr = const rEmpty

-- Recursive case 1: list is of type x ': xs.
instance {-# OVERLAPPABLE #-} (LookupH t x as (Remove t x as), Rearrange t as xs) =>
    Rearrange t as (x ': xs) where
        rearr env = lookupH env $ rearr env

-- Recursive case 2: list is of type x ': xs, and x is a HList itself.
instance {-# OVERLAPPING #-} (Rearrange t as xs, Rearrange t as ys) =>
    Rearrange t as (t xs ': ys) where
        rearr env = rCons (rearr env) (rearr env)

-- **********
-- RearrangeDel rearranges arbitrary HLists into other HLists (with deletion)
-- **********

class Rearrangeable t => RearrangeDel t a b c | a b -> c where
    rDel :: t a -> (t b, t c)

type Permute t a b = RearrangeDel t a b '[]

permute :: Permute t a b => t a -> t b
permute = fst . rDel

-- Base case: result is empty.
instance Rearrangeable t => RearrangeDel t a '[] a where
    rDel l = (rEmpty, l)

-- Recursive case 1: list is of type x ': xs.
instance {-# OVERLAPPABLE #-} (a' ~ Remove t x a, LookupH t x a a', RearrangeDel t a' xs a'') =>
    RearrangeDel t a (x ': xs) a'' where
        rDel env = let (prependRes, env') = removeH env
                       (rest, env'') = rDel env'
                    in (prependRes rest, env'')

-- Recursive case 2: list is of type x ': xs, and x is a HList itself.
instance {-# OVERLAPPING #-} (RearrangeDel t a xs a', RearrangeDel t a' ys a'') =>
    RearrangeDel t a (t xs ': ys) a'' where
        rDel env = let (left, env') = rDel env
                       (right, env'') = rDel env'
                    in (rCons left right, env'')

-- **********
-- LookupH allows elements of a given type to be retrieved from a HList.
-- **********

class LookupH t x env env' | x env -> env' where
    lookupH :: t env -> (t xs -> t (x ': xs))
    removeH :: t env -> (t xs -> t (x ': xs), t env')

-- Base case: the result is at the head of the input, so take it.
instance {-# OVERLAPPING #-} Rearrangeable t => LookupH t x (x ': xs) xs where
    lookupH h = rConsToHead h
    removeH h = (rConsToHead h, rTail h)

-- Recursive case: we don't immediately match, so we may need to explore the
-- head of the list further (if it is itself a HList that contains the target)
-- or just skip it.
instance {-# OVERLAPPABLE #-} (r ~ ContainsOrSingle t x y,
    Nest t r x (y ': xs) ys) =>
    LookupH t x (y ': xs) ys where
        lookupH = lookupHNest @t @r @x @(y ': xs) @ys
        removeH = removeHNest @t @r @x @(y ': xs) @ys

-- Nest determines whether the first element is a list and whether we should
-- investigate within it (using the result of ContainsOrSingle).
class Rearrangeable t => Nest t (res :: COS) x env env' | x env -> env' where
    lookupHNest :: t env -> (t xs -> t (x ': xs))
    removeHNest :: t env -> (t xs -> t (x ': xs), t env')

-- If it is a single element, skip over it.
instance (Rearrangeable t, LookupH t x xs ys) =>
    Nest t 'Single x (y ': xs) (y ': ys) where
        lookupHNest h = lookupH (rTail h)
        removeHNest h =
            let (res, rest) = removeH (rTail h)
            in (res, rConsToHead h rest)

-- If it is a HList and Contains found our target, explore that target.
instance (Rearrangeable t, LookupH t x xs xs') =>
    Nest t ('IsContained 'True) x (t xs ': ys) (t xs' ': ys) where
        lookupHNest h = lookupH (rHead h)
        removeHNest h =
            let (res, rest) = removeH (rHead h)
            in (res, rCons rest $ rTail h)

-- If it is a HList and Contains did not find our target, skip over it.
instance (Rearrangeable t, LookupH t x ys ys') =>
    Nest t ('IsContained 'False) x (t xs ': ys) (t xs ': ys') where
        lookupHNest h = lookupH (rTail h)
        removeHNest h =
            let (res, rest) = removeH (rTail h)
            in (res, rConsToHead h rest)