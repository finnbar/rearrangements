module Util where

type family Without (xs :: [*]) (x :: *) :: [*] where
  Without '[] x = '[]
  Without (x ': xs) x = Without xs x
  Without (x ': xs) y = x ': Without xs y
