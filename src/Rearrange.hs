{-# LANGUAGE UndecidableInstances, ScopedTypeVariables #-}

module Rearrange where

import HList

import Language.Haskell.TH

-- IDEA: We know the list structure at compile-time, so want to built code that
-- rearranges that structure without the overheads of typeclasses.
-- We do not know the values in our HLists at compile-time.
-- NOTE: look up polymorphism through TH.

class RearrangeDel env target env' | env target -> env' where
    rDel :: Q (TExp (HList env -> (HList target, HList env')))

instance RearrangeDel env '[] env where
    rDel = [|| \l -> (HNil, l) ||]

instance {-# OVERLAPPABLE #-} (RearrangeDel env' target' env'',
    GetHListElem x env env') =>
    RearrangeDel env (x ': target') env'' where
        rDel = [|| \l ->
            let (x, l') = $$(getHListElem @x @env @env') l
                (xs, l'') = $$(rDel) l'
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