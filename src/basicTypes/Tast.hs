--------------------------------------------------------------------------------
-- (c) Tsitsimpis Ilias, 2011
--
-- Typed Abstract Syntax Tree for the Alan Language
--
-- Since the LLVM API is typed it's much easier to translate a typed abstract
-- syntax tree than an untyped abstract syntax tree. The Tast module contains
-- the definition of the typed AST. There are many ways to formulate type safe
-- abstract syntax trees. I've chosen to use GADTs.
--
--------------------------------------------------------------------------------

{-# LANGUAGE GADTs, ExistentialQuantification, PatternGuards #-}

module Tast where

import Uast
import Data.Int
import Data.Word
import Foreign.Ptr
import Control.Monad


data TFun a where
    FunBody ::        TType a -> Stmt   -> TFun a
    FunHead :: Ide -> TType a -> TFun b -> TFun (a->b)

data TType a where
    TTypeInt   :: TType Int32
    TTypeChar  :: TType Word8
    TTypeProc  :: TType ()
    TTypeArray :: TType a     -> TType (Ptr a)

data TExpr a where
    TExprInt    :: Int32   -> TExpr Int32
    TExprChar   :: Word8   -> TExpr Word8
    TExprString :: String  -> TExpr (Ptr Word8)
    TExprVal    :: Ide     -> TType a       -> TExpr a
    TExprValArr :: Ide     -> TType (Ptr a) -> TExpr Int32 -> TExpr a
    TExprFun    :: Ide     -> TType a       -> [TExpr b]   -> TExpr a
    TExprSign   :: Op      -> TExpr a       -> TExpr a
    TExprOp     :: TExpr a -> Op            -> TExpr a     -> TExpr a

data ATExpr = forall a . TExpr a ::: TType a

data AFun = forall a . AFun (TFun a) (TType a)

data AType = forall a. AType (TType a)

data Equal a b where
    Eq :: Equal a b

test :: TType a -> TType b -> Maybe (Equal a b)
test TTypeInt       TTypeInt       = return Eq
test TTypeChar      TTypeChar      = return Eq
test TTypeProc      TTypeProc      = return Eq
test (TTypeArray a) (TTypeArray b) = do
    Eq <- test a b
    return Eq
test _              _              = mzero

class Type a where
    theType :: TType a

instance Type Int32 where
    theType = TTypeInt

instance Type Word8 where
    theType = TTypeChar

instance Type () where
    theType = TTypeProc

instance (Type a) => Type (Ptr a) where
    theType = TTypeArray theType
