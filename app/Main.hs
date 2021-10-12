{-# LANGUAGE TemplateHaskell, DataKinds, FlexibleContexts, TypeApplications, GADTs, PackageImports #-}
{-# OPTIONS_GHC -ddump-splices -ddump-to-file #-}

module Main where

import Rearrange
import HList
import RunTcM
import "template-haskell" Language.Haskell.TH

list :: HList '[Int, Bool, ()]
list = 3 :+: True :+: () :+: HNil

-- problem: $$(rearrange) has no access to type information outside of itself.
-- is there a way to push its expansion as late as possible, so that it has the relevant type information?
list' :: (HList '[Bool, (), Int], HList '[])
list' = $$(rearrange @(HList '[Int, Bool, ()]) @(HList '[Bool, (), Int])) list

x :: Int
x = (+) three $$(printEnv $ letExpand threee)
    where three = 3

main :: IO ()
main = putStrLn $ show list'
