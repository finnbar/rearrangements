{-# LANGUAGE TemplateHaskell, DataKinds, FlexibleContexts, TypeApplications, GADTs #-}
{-# OPTIONS_GHC -ddump-splices -ddump-to-file #-}

module Main where

import Rearrange
import HList

list :: HList '[Int, Bool, ()]
list = 3 :+: True :+: () :+: HNil

-- due to the discussion in https://gitlab.haskell.org/ghc/ghc/-/issues/10271,
-- we have to specify the type directly with type annotations (even though
-- the types of list and list' should fully infer it)
list' :: HList '[Bool, (), Int]
list' = $$(rearrange @(HList '[Int, Bool, ()]) @(HList '[Bool, (), Int])) list

main :: IO ()
main = print list'

-- TODO: have all forms of rearrangement available as both TH and non-TH versions
-- also need to be able to generalise to other hlist-likes