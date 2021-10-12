{-# LANGUAGE BangPatterns, TypeApplications, ScopedTypeVariables #-}

module RunTcM where

import "template-haskell" Language.Haskell.TH.Syntax
import DynFlags
import TcRnMonad
import Unsafe.Coerce
import Outputable
import Pretty
import System.IO
import TcMType
import LiftType
import Type.Reflection

unsafeRunTcM :: TcM a -> Q a
unsafeRunTcM m = unsafeCoerce (\_ -> m)

debugQ :: (Show a) => a -> Q ()
debugQ = runIO . print

-- SPICY TRICK: replace a rearrange call with
-- let name = $$(rearrange) in name
-- and then check the expected type of name
-- _hopefully_ this will give us the expected type to fill the hole rather than
-- the type of rearrange.

-- 1. Replace expr with (let x = expr in x)
-- 2. Get the type of x.
-- 3. Add an annotation to expr.
letExpand :: Q (TExp a) -> Q (TExp a)
letExpand expr = do
    --let name = mkName "x"
    expr' <- [|| let x = $$(expr) in x ||]
    --info <- reify x
    return expr'

threee :: Q (TExp Int)
threee = [|| 3 ||]

printEnv :: Q (TExp a) -> Q (TExp a)
printEnv exp = do
    !flags <- unsafeRunTcM getDynFlags
    --runIO $ putStrLn "flags"
    let !style = defaultUserStyle flags
    loc <- unsafeRunTcM getLocalRdrEnv
    runIO $ printSDocLn PageMode flags stdout style (ppr loc) >> hFlush stdout
    exp