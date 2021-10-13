{-# LANGUAGE FlexibleContexts, PolyKinds #-}

module Data.HList where

-- TODO: make isomorphic to Set
data HList :: [*] -> * where
  HNil :: HList '[]
  (:+:) :: x -> HList xs -> HList (x ': xs)

infixr 5 :+:

instance Show (HList '[]) where
  show HNil = "HNil"

instance (Show x, Show (HList xs)) => Show (HList (x ': xs)) where
  show (x :+: xs) = show x ++ " :+: " ++ show xs
