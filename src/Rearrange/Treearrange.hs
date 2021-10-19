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

-- TODO: this is blatantly wrong.
class LookupH k x a a' where
    lookupH :: a -> (x, a')

-- Instance for lists which consist of lists

class LookupHNodes k (x :: *) (xs :: [*]) (xs' :: [*]) | x xs -> xs' where
    removeHNodes :: HList xs -> (HList ys -> HList (x ': ys), HList xs')
    getHNodes :: HList xs -> (HList ys -> HList (x ': ys))

-- TODO: this implementation. The challenge, as always, is to find out which
-- way (in a list) to go at compile time.

-- Instance for lists of nodes of kind k (no recursion)

class LookupHLeaves k (x :: k) (xs :: [k]) (xs' :: [k]) | x xs -> xs' where
    removeHLeaves :: HList xs -> (HList ys -> HList (x ': ys), HList xs')
    getHLeaves :: HList xs -> (HList ys -> HList (x ': ys))

instance LookupHLeaves k x (x ': xs) xs where
    removeHLeaves (x :+: xs) = ((x :+:), xs)
    getHLeaves (x :+: _) = (x :+:)

instance (LookupHLeaves k x xs xs') =>
    LookupHLeaves k x (y ': xs) (y ': xs') where
        removeHLeaves (x :+: xs) =
            let (res, rest) = removeHLeaves xs
            in (res, x :+: rest)
        getHLeaves (_ :+: xs) = getHLeaves xs