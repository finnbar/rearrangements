{-# LANGUAGE FlexibleContexts #-}

module HList where

data HList :: [*] -> * where
  HNil :: HList '[]
  (:+:) :: x -> HList xs -> HList (x ': xs)

infixr 5 :+:

instance Show (HList '[]) where
  show HNil = "HNil"

instance (Show x, Show (HList xs)) => Show (HList (x ': xs)) where
  show (x :+: xs) = show x ++ " :+: " ++ show xs
