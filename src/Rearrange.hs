{-# LANGUAGE UndecidableInstances, ScopedTypeVariables, FlexibleContexts #-}

module Rearrange where

import Data.HList

import Language.Haskell.TH

-- | rearrange performs rearrangement without deletion.

rearrange :: forall k (a :: [k]) (b :: [k]). (Rearrange a b)
    => Q (TExp (HList a -> HList b))
rearrange = rearr @a @b

class Rearrange inp out where
    rearr :: Q (TExp (HList inp -> HList out))

instance Rearrange env '[] where
    rearr = [|| const HNil ||]

instance {-# OVERLAPPABLE #-} (Rearrange env target, HListElem x env env')
    => Rearrange env (x ': target) where
        rearr = [|| \l ->
            $$(getHListElem) l :+: $$(rearr) l ||]

instance {-# OVERLAPPING #-} (Rearrange env head, Rearrange env tail)
    => Rearrange env (HList head ': tail) where
        rearr = [|| \l ->
            $$(rearr) l :+: $$(rearr) l ||]

-- | rearrangeDel performs rearrangement with deletion.

rearrangeDel :: forall k (a :: [k]) (b :: [k]) (c :: [k]). (RearrangeDel a b c)
    => Q (TExp (HList a -> (HList b, HList c)))
rearrangeDel = rDel @a @b @c

type Permute env target = RearrangeDel env target '[]
permute :: forall k (a :: [k]) (b :: [k]). (Permute a b)
    => Q (TExp (HList a -> HList b))
permute = [|| fst . $$(rDel) ||]

class RearrangeDel env target env' | env target -> env' where
    rDel :: Q (TExp (HList env -> (HList target, HList env')))

instance RearrangeDel env '[] env where
    rDel = [|| \l -> (HNil, l) ||]

instance {-# OVERLAPPABLE #-} (RearrangeDel env' target' env'',
    HListElem x env env') =>
    RearrangeDel env (x ': target') env'' where
        rDel = [|| \l ->
            let (x, l') = $$(removeHListElem @x @env @env') l
                (xs, l'') = $$(rDel @env' @target' @env'') l'
            in (x :+: xs, l'') ||]

instance {-# OVERLAPPING #-} (RearrangeDel env head env',
    RearrangeDel env' target' env'') =>
    RearrangeDel env (HList head ': target') env'' where
        rDel = [|| \l ->
            let (head', l') = $$(rDel) l
                (tail', l'') = $$(rDel) l'
            in (head' :+: tail', l'') ||]

-- | removeHListElem retrieves the first element of type x from a list of xs.

class HListElem x env env' | x env -> env' where
    removeHListElem :: Q (TExp (HList env -> (x, HList env')))
    getHListElem :: Q (TExp (HList env -> x))

instance {-# OVERLAPPING #-} HListElem x (x ': xs) xs where
    removeHListElem = [|| \(x :+: xs) -> (x, xs) ||]
    getHListElem = [|| \(x :+: _) -> x ||]

instance (HListElem x inp' out', out ~ (o ': out')) =>
    HListElem x (o ': inp') out where
        removeHListElem = [|| \(y :+: xs) ->
            let (res, rest) = $$(removeHListElem @x @inp' @out') xs
            in (res, y :+: rest) ||]
        getHListElem = [|| \(_ :+: xs) ->
            $$(getHListElem @x @inp' @out') xs ||]