{-# LANGUAGE TemplateHaskell, DataKinds, FlexibleContexts, TypeApplications, GADTs #-}
{-# OPTIONS_GHC -ddump-splices -ddump-to-file #-}

module Main where

import Rearrange.TH
import Rearrange.Typeclass
import Data.HList

list :: HList '[Int, Bool, ()]
list = 3 :+: True :+: () :+: HNil

list' :: HList '[Bool, (), Int]
list' = rearrange list

-- due to the discussion in https://gitlab.haskell.org/ghc/ghc/-/issues/10271,
-- we have to specify the type directly with type annotations (even though
-- the types of list and list' should fully infer it)
list'' :: HList '[Bool, (), Int]
list'' = $$(rearrangeTH @'[Int, Bool, ()] @'[Bool, (), Int]) list

main :: IO ()
main = print list' >> print list''