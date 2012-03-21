--------------------------------------------------------------------------------
-- (c) Tsitsimpis Ilias, 2011-2012
--
-- Generate llvm code
--
-- This module used llvm bindings for haskell to
-- compile our Typed AST into llvm code
--
--------------------------------------------------------------------------------

{-# LANGUAGE GADTs, RankNTypes, ScopedTypeVariables #-}
module LlvmCodeGen (compile) where

import TypedAst
import SrcLoc
import Outputable

import LLVM.Core

compile :: [TAst] -> CodeGenModule ()
compile tasts = do
    mapM_ foo tasts
    return ()

foo :: ADefFun -> CodeGenModule ()
foo (ADefFun tdef _) = do
    translate tdef

translate :: forall a. (IsFunction a) => TDefFun a -> CodeGenModule ()
translate tdef = do
    (_::Function a) <- newNamedFunction ExternalLinkage (funName tdef)
    return ()

funName :: forall a. (IsFunction a) => TDefFun a -> String
funName (TDefPar _ _ tdef) = funName tdef
funName (TDefFunL lide _ _ _) = unLoc lide
funName _ = panic "LlvmCodeGen.funName got unexpected input"
