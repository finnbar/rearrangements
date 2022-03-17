{-# LANGUAGE TemplateHaskell, DataKinds, FlexibleContexts, TypeApplications, GADTs #-}
{-# OPTIONS_GHC -ddump-splices -ddump-to-file #-}

module Main where

import Rearrange.TH
import Rearrange.Typeclass
import Data.HList

list :: HList '[Int, Bool, ()]
list = 3 :+: True :+: () :+: HNil

-- TODO: test lists within lists etc.

list' :: HList '[Int, (), Bool]
list' = fst $ rDel list

-- due to the discussion in https://gitlab.haskell.org/ghc/ghc/-/issues/10271,
-- we have to specify the type directly with type annotations (even though
-- the types of list and list' should fully infer it)
list'' :: HList '[Bool, (), Int]
list'' = $$(rearrangeTH @'[Int, Bool, ()] @'[Bool, (), Int]) list

multiList :: HList '[HList '[Bool, ()], Int]
multiList = (True :+: () :+: HNil) :+: 3 :+: HNil

multiList' :: HList '[(), Int, Bool]
multiList' = rearr multiList

multiList'' :: HList '[(), HList '[Bool, Int]]
multiList'' = rearr multiList

multiList''' :: HList '[HList '[HList '[Int, Bool], ()]]
multiList''' = rearr multiList

main :: IO ()
main = print list' >> print list'' >> print multiList' >> print multiList'' >> print multiList'''