module Data.HMonoid where

import Data.HList

import Language.Haskell.TH

class HMonoid a where
    hEmpty :: a '[]
    (<<>>) :: x -> a xs -> a (x ': xs)
    hUncons :: a (x ': xs) -> (x, a xs)

    hHead :: a (x ': xs) -> x
    hHead = fst . hUncons
    hTail :: a (x ': xs) -> a xs
    hTail = snd . hUncons

    hEmptyTH :: Q (TExp (a '[]))
    hEmptyTH = [|| hEmpty ||]
    hCombine :: Q (TExp (x -> a xs -> a (x ': xs)))
    hCombine = [|| (<<>>) ||]
    hUnconsTH :: Q (TExp (a (x ': xs) -> (x, a xs)))
    hUnconsTH = [|| hUncons ||]
    hHeadTH :: Q (TExp (a (x ': xs) -> x))
    hHeadTH = [|| hHead ||]
    hTailTH :: Q (TExp (a (x ': xs) -> a xs))
    hTailTH = [|| hTail ||]

instance HMonoid HList where
    hEmpty = HNil
    (<<>>) = (:+:)
    hUncons (x :+: xs) = (x, xs)
    hEmptyTH = [|| HNil ||]
    hCombine = [|| (:+:) ||]
    hUnconsTH = [|| \(x :+: xs) -> (x, xs) ||]
    hHeadTH = [|| \(x :+: _) -> x ||]
    hTailTH = [|| \(_ :+: xs) -> xs ||]