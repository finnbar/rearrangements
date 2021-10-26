{-# LANGUAGE UndecidableInstances, ScopedTypeVariables,
    FlexibleContexts, AllowAmbiguousTypes #-}

module Rearrange.Treearrange where

import Data.HList

-- **********
-- Rearrange arbitrary HLists into other HLists (no deletion)
-- **********

class Rearrange a b where
    rearr :: a -> b

-- Base case: result is empty.
instance Rearrange a (HList '[]) where
    rearr = const HNil

-- Recursive case 1: list is of type x ': xs.
instance {-# OVERLAPPABLE #-} (LookupH x a a', Rearrange a (HList xs)) =>
    Rearrange a (HList (x ': xs)) where
        rearr env = lookupH env $ rearr env

-- Recursive case 2: list is of type x ': xs, and x is a HList itself.
instance {-# OVERLAPPING #-} (Rearrange a (HList xs), Rearrange a (HList ys)) =>
    Rearrange a (HList (HList xs ': ys)) where
        rearr env = rearr env :+: rearr env

-- *********
-- RearrangeDel rearranges arbitrary HLists into other HLists (with deletion)
-- *********

class RearrangeDel a b c | a b -> c where
    rDel :: a -> (b, c)

-- Base case: result is empty.
instance RearrangeDel a (HList '[]) a where
    rDel l = (HNil, l)

-- Recursive case 1: list is of type x ': xs.
instance {-# OVERLAPPABLE #-} (LookupH x a a', RearrangeDel a' (HList xs) a'') =>
    RearrangeDel a (HList (x ': xs)) a'' where
        rDel env = let (prependRes, env') = removeH env
                       (rest, env'') = rDel env'
                    in (prependRes rest, env'')

-- Recursive case 2: list is of type x ': xs, and x is a HList itself.
instance {-# OVERLAPPING #-} (RearrangeDel a (HList xs) a', RearrangeDel a' (HList ys) a'') =>
    RearrangeDel a (HList (HList xs ': ys)) a'' where
        rDel env = let (left, env') = rDel env
                       (right, env'') = rDel env'
                    in (left :+: right, env'')

-- *********
-- LookupH allows elements of a given type to be retrieved from a HList.
-- *********

class LookupH x env env' | x env -> env' where
    lookupH :: env -> (HList xs -> HList (x ': xs))
    removeH :: env -> (HList xs -> HList (x ': xs), env')

-- Base case: the result is at the head of the input, so take it.
instance {-# OVERLAPPING #-} LookupH x (HList (x ': xs)) (HList xs) where
    lookupH (x :+: _) = (x :+:)
    removeH (x :+: xs) = ((x :+:), xs)

-- Type-level Or, for simplicity.
type family Or (x :: Bool) (y :: Bool) :: Bool where
    Or 'False 'False = 'False
    Or _ _ = 'True

-- Determines whether a type is present anywhere within nested HLists.
type family Contains (x :: k) (l :: *) :: Bool where
    Contains x (HList '[]) = 'False
    Contains x (HList (x ': _)) = 'True
    Contains x (HList (HList es ': xs)) =
        Or (Contains x (HList es)) (Contains x (HList xs))
    Contains x (HList (y ': xs)) = Contains x (HList xs)

data COS = IsContained Bool | Single

type family ContainsOrSingle (x :: k) (l :: *) :: COS where
    ContainsOrSingle x (HList xs) = 'IsContained (Contains x (HList xs))
    ContainsOrSingle x _ = 'Single

-- Recursive case: we don't immediately match, so we may need to explore the
-- head of the list further (if it is itself a HList that contains the target)
-- or just skip it.
instance {-# OVERLAPPABLE #-} (r ~ ContainsOrSingle x y,
    Nest r x (HList (y ': xs)) (HList (y ': ys))) =>
    LookupH x (HList (y ': xs)) (HList (y ': ys)) where
        lookupH = lookupHNest @r
        removeH = removeHNest @r

-- Nest determines whether the first element is a list and whether we should
-- investigate within it (using the result of ContainsOrSingle).
class Nest (res :: COS) x env env' | x env -> env' where
    lookupHNest :: env -> (HList xs -> HList (x ': xs))
    removeHNest :: env -> (HList xs -> HList (x ': xs), env')

-- If it is a single element, skip over it.
instance (LookupH x (HList xs) (HList ys)) =>
    Nest 'Single x (HList (y ': xs)) (HList (y ': ys)) where
        lookupHNest (_ :+: xs) = lookupH xs
        removeHNest (x :+: xs) =
            let (res, rest) = removeH xs
            in (res, x :+: rest)

-- If it is a HList and Contains found our target, explore that target.
instance (LookupH x (HList xs) (HList xs')) =>
    Nest ('IsContained 'True) x (HList (HList xs ': ys)) (HList (HList xs' ': ys)) where
        lookupHNest (x :+: _) = lookupH x
        removeHNest (x :+: xs) =
            let (res, rest) = removeH x
            in (res, rest :+: xs)

-- If it is a HList and Contains did not find our target, skip over it.
instance (LookupH x (HList ys) (HList ys')) =>
    Nest ('IsContained 'False) x (HList (HList xs ': ys)) (HList (HList xs ': ys')) where
        lookupHNest (_ :+: xs) = lookupH xs
        removeHNest (x :+: xs) =
            let (res, rest) = removeH xs
            in (res, x :+: rest)