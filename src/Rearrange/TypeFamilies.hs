{-# LANGUAGE UndecidableInstances #-}

module Rearrange.TypeFamilies where

type family If (b :: Bool) (x :: k) (y :: k) where
    If 'True t f = t
    If 'False t f = f

type family Remove (t :: [k] -> *) (x :: k) (xs :: [k]) :: [k] where
    Remove t x (t xs ': xs') = If (Contains t x (t xs))
        (t (Remove t x xs) ': xs') (t xs ': Remove t x xs')
    Remove t x (x ': xs) = xs
    Remove t x (y ': xs) = y ': Remove t x xs

-- Type-level Or, for simplicity.
type family Or (x :: Bool) (y :: Bool) :: Bool where
    Or 'False 'False = 'False
    Or _ _ = 'True

-- Determines whether a type is present anywhere within nested t.
type family Contains (t :: [*] -> *) (x :: *) (l :: *) :: Bool where
    Contains t x (t '[]) = 'False
    Contains t x (t (x ': _)) = 'True
    Contains t x (t (t es ': xs)) =
        Or (Contains t x (t es)) (Contains t x (t xs))
    Contains t x (t (y ': xs)) = Contains t x (t xs)

data COS = IsContained Bool | Single

type family ContainsOrSingle (t :: [*] -> *) x (l :: *) :: COS where
    ContainsOrSingle t x (t xs) = 'IsContained (Contains t x (t xs))
    ContainsOrSingle t x _ = 'Single

type family RemoveAll (t :: [k] -> *) (xs :: [k]) (li :: [k]) :: [k] where
    RemoveAll t '[] li = li
    RemoveAll t (x ': xs) li = RemoveAll t xs (Remove t x li)