{-# LANGUAGE UndecidableInstances, ScopedTypeVariables,
    FlexibleContexts, AllowAmbiguousTypes #-}

module Rearrange.Typeclass where

import Rearrange.Rearrangeable
import Rearrange.TypeFamilies
import Data.Kind (Type)

-- **********
-- Rearrange arbitrary HLists into other HLists (no deletion)
-- **********

-- POSSIBLE FUTURE WORK: Allow kind-poly rearrangements - e.g.
-- Rearrange Set st '[Set s, Set t] doesn't currently work due to the
-- different kinds of the input and output.

-- Rearrange allows a Rearrangeable structure to be rearranged into that same
-- structure but with the elements in a different order, as determined by the
-- type of that structure.
class Rearrangeable t => Rearrange (t :: [k] -> Type) (as :: [k]) (bs :: [k]) where
    rearr :: t as -> t bs

-- Base case: result is empty.
instance Rearrangeable t => Rearrange t as '[] where
    rearr = const rEmpty

-- Recursive case 1: list is of type x ': xs - so just look for an element of
-- type x in the input and use that.
instance {-# OVERLAPPABLE #-} (LookupH t x as (Remove t x as), Rearrange t as xs) =>
    Rearrange t as (x ': xs) where
        rearr env = lookupH env $ rearr env

-- Recursive case 2: output is of type x ': xs, and x is a structure itself, so
-- we need to first build that structure before continuing down the list.
-- i.e. if we have HList '[HList '[Bool, Int], ()], then we need to first
-- rearrange our input into a HList '[Bool, Int] so we can put it at the front.
instance {-# OVERLAPPING #-} (RearrangeableStar t, Rearrange t as xs, Rearrange t as ys) =>
    Rearrange t as (t xs ': ys) where
        rearr env = rCons (rearr env) (rearr env)

-- **********
-- RearrangeDel rearranges arbitrary HLists into other HLists (with deletion)
-- **********

-- RearrangeDel is just like Rearrange, except it only ever uses a member of
-- the input list once, "deleting" it once it it used.
class Rearrangeable t => RearrangeDel t a b c | a b -> c where
    rearrDel :: t a -> (t b, t c)

-- Permute and RDel avoid having to specify the type c.
type Permute t a b = RearrangeDel t a b '[]
type RDel t a b = RearrangeDel t a b (RemoveAll t b a)

permute :: Permute t a b => t a -> t b
permute = fst . rearrDel

rDel :: RDel t a b => t a -> t b
rDel = fst . rearrDel

-- Base case: result is empty.
instance Rearrangeable t => RearrangeDel t a '[] a where
    rearrDel l = (rEmpty, l)

-- Recursive case 1: list is of type x ': xs, so find the first element of type
-- x in the input, use that in the output and remove it from the input.
-- NOTE: The Rearrangeable t constraint looks redundant (it is implied by RearrangeDel), but
-- -Wloopy-superclass-solve asks that we make the constraint explicit.
instance {-# OVERLAPPABLE #-} (Rearrangeable t, a' ~ Remove t x a, LookupH t x a a', RearrangeDel t a' xs a'') =>
    RearrangeDel t a (x ': xs) a'' where
        rearrDel env = let (prependRes, env') = removeH env
                           (rest, env'') = rearrDel env'
                        in (prependRes rest, env'')

-- Recursive case 2: list is of type x ': xs, and x is a structure itself, so
-- we need to first build that structure.
instance {-# OVERLAPPING #-} (Rearrangeable t, RearrangeableStar t, RearrangeDel t a xs a', RearrangeDel t a' ys a'') =>
    RearrangeDel t a (t xs ': ys) a'' where
        rearrDel env = let (left, env') = rearrDel env
                           (right, env'') = rearrDel env'
                        in (rCons left right, env'')

-- **********
-- LookupH allows elements of a given type to be retrieved from a HList.
-- **********

-- Lookup an element of type x from env within a structure of type t, giving
-- env' once that element is removed from env.
-- Note here that the types are a bit awkward to deal with the fact that we
-- cannot just return a value of type x since x :: k.
class LookupH (t :: [k] -> Type) (x :: k) (env :: [k]) (env' :: [k]) | x env -> env' where
    lookupH :: t env -> (t xs -> t (x ': xs))
    removeH :: t env -> (t xs -> t (x ': xs), t env')

-- Base case: the result is at the head of the input, so take it.
instance {-# OVERLAPPING #-} Rearrangeable t => LookupH t x (x ': xs) xs where
    lookupH h = rConsToHead h
    removeH h = (rConsToHead h, rTail h)

-- Recursive case: we don't immediately match, so we may need to explore the
-- head of the list further (if it is itself a structure that contains the
-- target) or just skip it.
-- This uses ContainsOrSingle, which checks whether our target is within the
-- head of the explored list, thus informing which Nest definition we use.
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

-- If it is a single element (i.e. not a structure), skip over it since it is
-- not the target.
instance (Rearrangeable t, LookupH t x xs ys) =>
    Nest t 'Single x (y ': xs) (y ': ys) where
        lookupHNest h = lookupH (rTail h)
        removeHNest h =
            let (res, rest) = removeH (rTail h)
            in (res, rConsToHead h rest)

-- If it is a structure and Contains found our target, explore that target.
instance (Rearrangeable t, RearrangeableStar t, LookupH t x xs xs') =>
    Nest t ('IsContained 'True) x (t xs ': ys) (t xs' ': ys) where
        lookupHNest h = lookupH (rHead h)
        removeHNest h =
            let (res, rest) = removeH (rHead h)
            in (res, rCons rest $ rTail h)

-- If it is a structure and Contains did not find our target, skip over it.
instance (Rearrangeable t, LookupH t x ys ys') =>
    Nest t ('IsContained 'False) x (t xs ': ys) (t xs ': ys') where
        lookupHNest h = lookupH (rTail h)
        removeHNest h =
            let (res, rest) = removeH (rTail h)
            in (res, rConsToHead h rest)