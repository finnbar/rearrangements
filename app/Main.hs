{-# LANGUAGE TemplateHaskell, DataKinds, FlexibleContexts, TypeApplications, GADTs #-}
{-# OPTIONS_GHC -ddump-splices -ddump-to-file #-}

module Main where

import Rearrange
import HList

list :: HList '[Int, Bool, ()]
list = 3 :+: True :+: () :+: HNil

list' :: (HList '[Bool, (), Int], HList '[])
list' = --let rearr = $$(rDel) :: HList '[Int, Bool, ()] -> (HList '[Bool, (), Int], HList '[])
        -- in fst $ rearr list
        $$(rDel @('[Int, Bool, ()]) @('[Bool, (), Int])) list

main :: IO ()
main = putStrLn $ show list'
