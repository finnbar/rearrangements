{-# LANGUAGE UndecidableInstances, ScopedTypeVariables #-}

module Rearrange where

import HList

import "template-haskell" Language.Haskell.TH
import LiftType
import Type.Reflection

-- IDEA: We know the list structure at compile-time, so want to built code that
-- rearranges that structure without the overheads of typeclasses.
-- We do not know the values in our HLists at compile-time.
-- NOTE: look up polymorphism through TH.

-- Takes an expr and returns a quote with that type annotation.
-- typed :: forall tx ty. (Typeable tx, Typeable ty) => Exp -> tx -> ty -> Q Exp
-- typed e _ _ = return (AppTypeE e (App ArrowT (liftType @tx) (liftType @ty)))

rearrange :: forall x y z a b c. (RearrangeDel a b c, x ~ HList a, y ~ HList b, z ~ HList c)
    => Q (TExp (x -> (y, z)))
rearrange = rDel @a @b @c

class RearrangeDel env target env' | env target -> env' where
    rDel :: Q (TExp (HList env -> (HList target, HList env')))

instance RearrangeDel env '[] env where
    rDel = [|| \l -> (HNil, l) ||]

instance {-# OVERLAPPABLE #-} (RearrangeDel env' target' env'',
    GetHListElem x env env') =>
    RearrangeDel env (x ': target') env'' where
        rDel = [|| \l ->
            let (x, l') = $$(getHListElem @x @env @env') l
                (xs, l'') = $$(rDel @env' @target' @env'') l'
            in (x :+: xs, l'') ||]

instance {-# OVERLAPPING #-} (RearrangeDel env head env',
    RearrangeDel env' target' env'') =>
    RearrangeDel env (HList head ': target') env'' where
        rDel = [|| \l ->
            let (head', l') = $$(rDel) l
                (tail', l'') = $$(rDel) l'
            in (head' :+: tail', l'') ||]

class GetHListElem x env env' | x env -> env' where
    getHListElem :: Q (TExp (HList env -> (x, HList env')))

instance {-# OVERLAPPING #-} GetHListElem x (x ': xs) xs where
    getHListElem = [|| \(x :+: xs) -> (x, xs) ||]

instance (GetHListElem x inp' out', out ~ (o ': out')) =>
    GetHListElem x (o ': inp') out where
        getHListElem = [|| \list ->
            let (y :+: xs) = list
                (res, rest) = $$(getHListElem) xs
            in (res, y :+: rest) ||]