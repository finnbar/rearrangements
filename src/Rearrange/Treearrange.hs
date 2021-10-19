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

-- Flattening of nested HLists into a single HList

{-
class FlattenNestedHLists k xs (ys :: [k]) | xs -> ys where
    flattenH :: HList xs -> HList ys

instance FlattenNestedHLists k '[] '[] where
    flattenH = const HNil

instance {-# OVERLAPPING #-} (zs ~ Append ys' xs', FlattenNestedHLists k xs xs',
    FlattenNestedHLists k ys ys')
    => FlattenNestedHLists k (HList ys ': xs) zs where
        flattenH (list :+: lists) = flattenH @k @ys @ys' list
            `hAppend` flattenH @k @xs @xs' lists

instance {-# OVERLAPPABLE #-} (FlattenNestedHLists k xs ys)
    => FlattenNestedHLists k (x ': xs) (x ': ys) where
        flattenH (el :+: list) = el :+: flattenH list
-}

class FlattenNestedHLists x y | x -> y where
    flattenH :: x -> y

instance FlattenNestedHLists (HList ('[] @k)) (HList ('[] @k)) where
    flattenH = const HNil

instance {-# OVERLAPPING #-} (zs ~ Append ys' xs', FlattenNestedHLists (HList xs) (HList xs'),
    FlattenNestedHLists (HList ys) (HList ys'))
    => FlattenNestedHLists (HList (HList ys ': xs)) (HList zs) where
        flattenH (list :+: lists) = flattenH @(HList ys) @(HList ys') list
            `hAppend` flattenH @(HList xs) @(HList xs') lists

instance {-# OVERLAPPING #-} (FlattenNestedHLists (HList ys) (HList ys'))
    => FlattenNestedHLists (HList '[HList ys]) (HList ys') where
        flattenH (list :+: _) = flattenH @(HList ys) @(HList ys') list

instance {-# OVERLAPPABLE #-} (FlattenNestedHLists (HList xs) (HList ys))
    => FlattenNestedHLists (HList (x ': xs)) (HList (x ': ys)) where
        flattenH (el :+: list) = el :+: flattenH list