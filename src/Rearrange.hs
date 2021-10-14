{-# LANGUAGE UndecidableInstances, ScopedTypeVariables, FlexibleContexts #-}

module Rearrange where

import Data.HList

import Language.Haskell.TH

-- | rearrange performs rearrangement without deletion.

rearrange :: forall (a :: [*]) (b :: [*]). (Rearrange * a b)
    => Q (TExp (HList a -> HList b))
rearrange = rearr @(*) @a @b

class Rearrange k (inp :: [k]) (out :: [k]) where
    rearr :: Q (TExp (HList inp -> HList out))

instance Rearrange k env '[] where
    rearr = [|| const HNil ||]

instance {-# OVERLAPPABLE #-} (Rearrange k env target, HListElem k x env env')
    => Rearrange k env (x ': target) where
        rearr = [|| \l ->
            $$(getHListElem) l $ $$(rearr) l ||]

instance {-# OVERLAPPING #-} (Rearrange * env head, Rearrange * env tail)
    => Rearrange * env (HList head ': tail) where
        rearr = [|| \l ->
            $$(rearr) l :+: $$(rearr) l ||]

-- | rearrangeDel performs rearrangement with deletion.

rearrangeDel :: forall (a :: [*]) (b :: [*]) (c :: [*]). (RearrangeDel * a b c)
    => Q (TExp (HList a -> (HList b, HList c)))
rearrangeDel = rDel @(*) @a @b @c

type Permute k env target = RearrangeDel k env target '[]
permute :: forall k (a :: [k]) (b :: [k]). (Permute k a b)
    => Q (TExp (HList a -> HList b))
permute = [|| fst . $$(rDel) ||]

class RearrangeDel k (env :: [k]) (target :: [k]) (env' :: [k]) | env target -> env' where
    rDel :: Q (TExp (HList env -> (HList target, HList env')))

instance RearrangeDel k env '[] env where
    rDel = [|| \l -> (HNil, l) ||]

instance {-# OVERLAPPABLE #-} (RearrangeDel k env' target' env'',
    HListElem k x env env') =>
    RearrangeDel k env (x ': target') env'' where
        rDel = [|| \l ->
            let (prependX, l') = $$(removeHListElem @k @x @env @env') l
                (xs, l'') = $$(rDel @k @env' @target' @env'') l'
            in (prependX xs, l'') ||]

instance {-# OVERLAPPING #-} (RearrangeDel * env head env',
    RearrangeDel * env' target' env'') =>
    RearrangeDel * env (HList head ': target') env'' where
        rDel = [|| \l ->
            let (head', l') = $$(rDel) l
                (tail', l'') = $$(rDel) l'
            in (head' :+: tail', l'') ||]

-- | removeHListElem retrieves the first element of type x from a list of xs.
-- Note that we return an prepending function instead of the value itself since
-- its kind can be non-* and we only ever use it to prepend to the rest of our
-- resulting list.

class HListElem k (x :: k) (env :: [k]) (env' :: [k]) | x env -> env' where
    removeHListElem :: Q (TExp (HList env -> (HList xs -> HList (x ': xs), HList env')))
    getHListElem :: Q (TExp (HList env -> (HList xs -> HList (x ': xs))))

instance {-# OVERLAPPING #-} HListElem k x (x ': xs) xs where
    removeHListElem = [|| \(x :+: xs) -> ((x :+:), xs) ||]
    getHListElem = [|| \(x :+: _) -> (x :+:) ||]

instance (HListElem k x inp' out', out ~ (o ': out')) =>
    HListElem k x (o ': inp') out where
        removeHListElem = [|| \(y :+: xs) ->
            let (res, rest) = $$(removeHListElem @k @x @inp' @out') xs
            in (res, y :+: rest) ||]
        getHListElem = [|| \(_ :+: xs) ->
            $$(getHListElem @k @x @inp' @out') xs ||]