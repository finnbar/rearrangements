{-# LANGUAGE UndecidableInstances, ScopedTypeVariables,
    FlexibleContexts, AllowAmbiguousTypes #-}

module Rearrange.Treearrange where

import Data.HList

-- IDEA: Want trees with k-kinded nodes to be treeted properly.
-- Nodes are HLists, leaves are types of kind k.
-- Challenge is rearranging from and to.

-- Need to allow our lookups to do tree traversal. The challenge here is giving
-- our trees the right kind, since they consist of [*] and [k] internally.
-- Note that rearrange currently iterates on the _output_ not the input.
-- (i.e. we essentially do rearr l = getFirstElem :+: rearr l.)
-- We likely want to stick with this pattern - it's just a kinding challenge.

-- TODO: work out how to get the kind involved. Proxy??
class RearrangeTree a b k where
    rearrTree :: a -> b

instance RearrangeTree a (HList '[]) k where
    rearrTree = const HNil

instance (LookupH k x a a', RearrangeTree a (HList xs) k)
    => RearrangeTree a (HList (x ': xs)) k where
    rearrTree env = lookupH env $ rearrTree @_ @_ @k env

instance (RearrangeTree a (HList xs) k, RearrangeTree a (HList ys) k)
    => RearrangeTree a (HList (HList xs ': ys)) k where
        rearrTree env = rearrTree @a @(HList xs) @k env
            :+: rearrTree @a @(HList ys) @k env

-- TODO: implement lookup with deletion (hard).
-- Idea: delete from the HList it exists in.
-- Then rearrange with deletion becomes very similar to lookup.
class LookupH k (x :: k) env env' | x env -> env' where
    lookupH :: env -> (HList xs -> HList (x ': xs))
    removeH :: env -> (HList xs -> HList (x ': xs), env')

-- When a HList consists of leaves of kind k:

instance {-# OVERLAPPING #-} LookupH k x (HList (x ': xs)) (HList xs) where
    lookupH (x :+: xs) = (x :+:)
    removeH (x :+: xs) = ((x :+:), xs)

instance {-# OVERLAPPABLE #-} (LookupH k x (HList xs) (HList ys)) =>
    LookupH k x (HList (y ': xs)) (HList (y ': ys)) where
        lookupH (_ :+: xs) = lookupH xs
        removeH (x :+: xs) =
            let (res, rest) = removeH xs
            in (res, x :+: rest)

-- When a HList consists of other HLists:

type family Contains (x :: k) (xs :: [k]) where
    Contains x '[] = 'False
    Contains x (x ': xs) = 'True
    Contains x (y ': xs) = Contains x xs

--type family Flatten 

instance (b ~ Contains x xs, LookupHIn b k x (HList (HList xs ': ys)) (HList out)) =>
    LookupH k x (HList (HList xs ': ys)) (HList out) where
        lookupH = lookupHIn @b
        removeH = removeHIn @b

class LookupHIn (b :: Bool) k (x :: k) env env' | x env -> env' where
    lookupHIn :: env -> (HList xs -> HList (x ': xs))
    removeHIn :: env -> (HList xs -> HList (x ': xs), env')

instance (LookupH k x (HList xs) (HList zs),
    out ~ Append ys zs) =>
    LookupHIn 'True k x (HList (HList xs ': ys)) (HList out) where
        lookupHIn (x :+: _) = lookupH x
        removeHIn (x :+: xs) =
            let (res, rest) = removeH x
            in (res, xs `hAppend` rest)

instance (LookupH k x (HList ys) (HList zs),
    out ~ Append ys zs) =>
    LookupHIn 'False k x (HList (HList xs ': ys)) (HList out) where
        lookupHIn (_ :+: xs) = lookupH xs
        removeHIn (x :+: xs) =
            let (res, rest) = removeH xs
            in (res, xs `hAppend` rest)