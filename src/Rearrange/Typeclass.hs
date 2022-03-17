{-# LANGUAGE UndecidableInstances, ScopedTypeVariables,
    FlexibleContexts, AllowAmbiguousTypes #-}

module Rearrange.Typeclass where

import Rearrange.Rearrangeable

-- TODO: will have to do really awkward stuff to make this kind-poly. (See LookupH.)
-- Then make type-level-sets point to this branch, remove the explicit kinds and add aliases for Nubable etc.
-- Finally, need to work out how to make the Set thing work. (New branch, so Dom can have a comparison.)

-- **********
-- Rearrange arbitrary HLists into other HLists (no deletion)
-- **********

class Rearrangeable t => Rearrange t as bs where
    rearr :: t as -> t bs

-- Base case: result is empty.
instance Rearrangeable t => Rearrange t as '[] where
    rearr = const rEmpty

type family If (b :: Bool) (x :: k) (y :: k) where
    If 'True t f = t
    If 'False t f = f

type family Remove (t :: [k] -> *) (x :: k) (xs :: [k]) :: [k] where
    Remove t x (t xs ': xs') = If (Contains t x (t xs))
        (t (Remove t x xs) ': xs') (t xs ': Remove t x xs')
    Remove t x (x ': xs) = xs
    Remove t x (y ': xs) = y ': Remove t x xs

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
    lookupH h = rCons (rHead h)
    removeH h = (rCons (rHead h), rTail h)

-- Type-level Or, for simplicity.
type family Or (x :: Bool) (y :: Bool) :: Bool where
    Or 'False 'False = 'False
    Or _ _ = 'True

-- Determines whether a type is present anywhere within nested HLists.
type family Contains (t :: [*] -> *) (x :: *) (l :: *) :: Bool where
    Contains t x (t '[]) = 'False
    Contains t x (t (x ': _)) = 'True
    Contains t x (t (t es ': xs)) =
        Or (Contains t x (t es)) (Contains t x (t xs))
    Contains t x (t (y ': xs)) = Contains t x (t xs)

data COS = IsContained Bool | Single

type family ContainsOrSingle (t :: [*] -> *) x (l :: *) :: COS where
    ContainsOrSingle t x (t xs) = 'IsContained (Contains t x (t xs))
    ContainsOrSingle t x _ = 'Single

-- Recursive case: we don't immediately match, so we may need to explore the
-- head of the list further (if it is itself a HList that contains the target)
-- or just skip it.
-- TODO: I feel like there's a way to implement this more simply, because this is currently quite cursed.
-- IDEA: remove the two instances of LookupH, just use ContainsOrSingle immediately.
instance {-# OVERLAPPABLE #-} (r ~ ContainsOrSingle t x y,
    Nest t r x (y ': xs) (y' ': ys)) =>
    LookupH t x (y ': xs) (y' ': ys) where
        lookupH = lookupHNest @t @r
        removeH = removeHNest @t @r

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
            in (res, rCons (rHead h) rest)

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
            in (res, rCons (rHead h) rest)