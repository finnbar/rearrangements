{-# LANGUAGE UndecidableInstances, ScopedTypeVariables, FlexibleContexts #-}

module Rearrange where

import HMonoid

import Language.Haskell.TH

-- | rearrange performs rearrangement without deletion.

rearrange :: forall p q a b t. (p ~ t a, q ~ t b,
    HMonoid t, Rearrange a b t)
    => Q (TExp (p -> q))
rearrange = rearr @a @b @t

class Rearrange inp out t where
    rearr :: Q (TExp (t inp -> t out))

instance HMonoid t => Rearrange env '[] t where
    rearr = [|| const $$(hEmptyTH @t) ||]

instance {-# OVERLAPPABLE #-} (Rearrange env target t,
    HMonoidElem x env env' t, HMonoid t)
    => Rearrange env (x ': target) t where
        rearr = [|| \l ->
            $$(hCombine @t) ($$(getHMonoidElem) l) ($$(rearr) l) ||]

instance {-# OVERLAPPING #-} (Rearrange env head t,
    Rearrange env tail t, HMonoid t)
    => Rearrange env (t head ': tail) t where
        rearr = [|| \l ->
            $$(hCombine @t) ($$(rearr) l) ($$(rearr) l) ||]

-- | rearrangeDel performs rearrangement with deletion.

rearrangeDel :: forall p q r a b c t. (p ~ t a, q ~ t b, r ~ t c,
    HMonoid t, RearrangeDel a b c t)
    => Q (TExp (p -> (q, r)))
rearrangeDel = rDel @a @b @c @t

type Permute env target t = RearrangeDel env target '[] t
permute :: Permute env target t => Q (TExp (t env -> t target))
permute = [|| fst . $$(rDel) ||]

class RearrangeDel env target env' t | env target -> env' where
    rDel :: Q (TExp (t env -> (t target, t env')))

instance HMonoid t => RearrangeDel env '[] env t where
    rDel = [|| \l -> ($$(hEmptyTH @t), l) ||]

instance {-# OVERLAPPABLE #-} (RearrangeDel env' target' env'' t,
    HMonoidElem x env env' t, HMonoid t) =>
    RearrangeDel env (x ': target') env'' t where
        rDel = [|| \l ->
            let (x, l') = $$(removeHMonoidElem @x @env @env') l
                (xs, l'') = $$(rDel @env' @target' @env'') l'
            in ($$(hCombine @t) x xs, l'') ||]

instance {-# OVERLAPPING #-} (RearrangeDel env head env' t,
    RearrangeDel env' target' env'' t, HMonoid t) =>
    RearrangeDel env (t head ': target') env'' t where
        rDel = [|| \l ->
            let (head', l') = $$(rDel) l
                (tail', l'') = $$(rDel) l'
            in ($$(hCombine @t) head' tail', l'') ||]

-- | removeHMonoidElem retrieves the first element of type x from a list of xs.

class HMonoidElem x env env' t | x env -> env' where
    removeHMonoidElem :: Q (TExp (t env -> (x, t env')))
    getHMonoidElem :: Q (TExp (t env -> x))

instance {-# OVERLAPPING #-} HMonoid t => HMonoidElem x (x ': xs) xs t where
    removeHMonoidElem = hUnconsTH
    getHMonoidElem = [|| $$(hHeadTH) ||]

instance (HMonoidElem x inp' out' t, out ~ (o ': out'), HMonoid t) =>
    HMonoidElem x (o ': inp') out t where
        removeHMonoidElem = [|| \list ->
            let (y, xs) = $$(hUnconsTH @t) list
                (res, rest) = $$(removeHMonoidElem) xs
            in (res, $$(hCombine @t) y rest) ||]
        getHMonoidElem = [|| 
            $$(getHMonoidElem) . $$(hTailTH) ||]