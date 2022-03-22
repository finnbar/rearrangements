{-# LANGUAGE UndecidableInstances, ScopedTypeVariables, FlexibleContexts #-}

module Rearrange.TH where

import Data.HList

import Language.Haskell.TH

-- | rearrange performs rearrangement without deletion.

rearrangeTH :: forall (a :: [*]) (b :: [*]). (RearrangeTH * a b)
    => Q (TExp (HList a -> HList b))
rearrangeTH = rearrTH @(*) @a @b

class RearrangeTH k (inp :: [k]) (out :: [k]) where
    rearrTH :: Q (TExp (HList inp -> HList out))

instance RearrangeTH k env '[] where
    rearrTH = [|| const HNil ||]

instance {-# OVERLAPPABLE #-} (RearrangeTH k env target, HListElemTH k x env env')
    => RearrangeTH k env (x ': target) where
        rearrTH = [|| \l ->
            $$(getHListElemTH) l $ $$(rearrTH) l ||]

instance {-# OVERLAPPING #-} (RearrangeTH * env head, RearrangeTH * env tail)
    => RearrangeTH * env (HList head ': tail) where
        rearrTH = [|| \l ->
            $$(rearrTH) l :+: $$(rearrTH) l ||]

-- | rearrangeDel performs rearrangement with deletion.

rearrangeDelTH :: forall (a :: [*]) (b :: [*]) (c :: [*]). (RearrangeDelTH * a b c)
    => Q (TExp (HList a -> (HList b, HList c)))
rearrangeDelTH = rearrDelTH @(*) @a @b @c

type PermuteTH k env target = RearrangeDelTH k env target '[]
permuteTH :: forall k (a :: [k]) (b :: [k]). (PermuteTH k a b)
    => Q (TExp (HList a -> HList b))
permuteTH = [|| fst . $$(rearrDelTH) ||]

class RearrangeDelTH k (env :: [k]) (target :: [k]) (env' :: [k]) | env target -> env' where
    rearrDelTH :: Q (TExp (HList env -> (HList target, HList env')))

instance RearrangeDelTH k env '[] env where
    rearrDelTH = [|| \l -> (HNil, l) ||]

instance {-# OVERLAPPABLE #-} (RearrangeDelTH k env' target' env'',
    HListElemTH k x env env') =>
    RearrangeDelTH k env (x ': target') env'' where
        rearrDelTH = [|| \l ->
            let (prependX, l') = $$(removeHListElemTH @k @x @env @env') l
                (xs, l'') = $$(rearrDelTH @k @env' @target' @env'') l'
            in (prependX xs, l'') ||]

instance {-# OVERLAPPING #-} (RearrangeDelTH * env head env',
    RearrangeDelTH * env' target' env'') =>
    RearrangeDelTH * env (HList head ': target') env'' where
        rearrDelTH = [|| \l ->
            let (head', l') = $$(rearrDelTH) l
                (tail', l'') = $$(rearrDelTH) l'
            in (head' :+: tail', l'') ||]

-- | removeHListElem retrieves the first element of type x from a list of xs.
-- Note that we return an prepending function instead of the value itself since
-- its kind can be non-* and we only ever use it to prepend to the rest of our
-- resulting list.

class HListElemTH k (x :: k) (env :: [k]) (env' :: [k]) | x env -> env' where
    removeHListElemTH :: Q (TExp (HList env -> (HList xs -> HList (x ': xs), HList env')))
    getHListElemTH :: Q (TExp (HList env -> (HList xs -> HList (x ': xs))))

instance {-# OVERLAPPING #-} HListElemTH k x (x ': xs) xs where
    removeHListElemTH = [|| \(x :+: xs) -> ((x :+:), xs) ||]
    getHListElemTH = [|| \(x :+: _) -> (x :+:) ||]

instance (HListElemTH k x inp' out', out ~ (o ': out')) =>
    HListElemTH k x (o ': inp') out where
        removeHListElemTH = [|| \(y :+: xs) ->
            let (res, rest) = $$(removeHListElemTH @k @x @inp' @out') xs
            in (res, y :+: rest) ||]
        getHListElemTH = [|| \(_ :+: xs) ->
            $$(getHListElemTH @k @x @inp' @out') xs ||]