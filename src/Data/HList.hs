{-# LANGUAGE FlexibleContexts, PolyKinds #-}

module Data.HList where

data HList (n :: [k]) where
  HNil :: HList '[]
  (:+:) :: x -> HList xs -> HList (x ': xs)

infixr 5 :+:

instance Show (HList '[]) where
  show HNil = "[]"

instance (Show e, Show' (HList s)) => Show (HList (e ': s)) where
  show (e :+: s) = "[" ++ show e ++ show' s ++ "]"

class Show' t where
  show' :: t -> String
instance Show' (HList '[]) where
  show' HNil = ""
instance (Show' (HList s), Show e) => Show' (HList (e ': s)) where
  show' (e :+: s) = ", " ++ show e ++ show' s

instance Eq (HList '[]) where
  (==) _ _ = True
instance (Eq e, Eq (HList s)) => Eq (HList (e ': s)) where
    (e :+: m) == (e' :+: m') = e == e' && m == m'

instance Ord (HList '[]) where
  compare _ _ = EQ
instance (Ord a, Ord (HList s)) => Ord (HList (a ': s)) where
  compare (a :+: as) (a' :+: as') = case compare a a' of
    EQ ->
      compare as as'
    other ->
      other

type family Append (xs :: [k]) (ys :: [k]) where
  Append '[] ys = ys
  Append (x ': xs) ys = x ': Append xs ys

hAppend :: HList xs -> HList ys -> HList (Append xs ys)
hAppend HNil ys = ys
hAppend (x :+: xs) ys = x :+: hAppend xs ys