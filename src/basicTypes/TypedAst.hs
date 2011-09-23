--------------------------------------------------------------------------------
-- (c) Tsitsimpis Ilias, 2011
--
-- Typed Abstract Syntax Tree for the Alan Language
--
-- Since the LLVM API is typed it's much easier to translate a typed abstract
-- syntax tree than an untyped abstract syntax tree. The TypedAst module
-- contains the definition of the typed AST. There are many ways to formulate
-- type safe abstract syntax trees.
-- I've chosen to use GADTs.
--
--------------------------------------------------------------------------------

{-# LANGUAGE GADTs, ExistentialQuantification, PatternGuards #-}

module TypedAst where

import UnTypedAst
import SrcLoc

import Data.Int
import Data.Word
import Foreign.Ptr
import Control.Monad


type LTDef a = Located (TDef a)

data TDef a where
    TDefFun  :: LIde -> TDef a   -> LTType b -> [LADef] -> TDef (a -> b)
    TDefFunE :: LIde             -> LTType b -> [LADef] -> TDef b
    TDefParH :: LIde -> Mode     -> LTType a -> TDef b  -> TDef (a -> b)
    TDefParT :: LIde -> Mode     -> LTType a            -> TDef a
    TDefVar  :: LIde -> LTType a                        -> TDef a

type LADef = Located ADef

data ADef = forall a . ADef (TDef a) (TType a)


type LTStmt = Located TStmt

data TStmt
    = TStmtNothing
    | TStmtAssign LAVariable LAExpr
    | TStmtCompound [LTStmt]
    | TStmtFun LIde [LAExpr]
    | TStmtIf LTCond LTStmt (Maybe LTStmt)
    | TStmtWhile LTCond LTStmt
    | TStmtReturn (Maybe LAExpr )


type LTExpr a = Located (TExpr a)

data TExpr a where
    TExprInt    :: Int32                               -> TExpr Int32
    TExprChar   :: Word8                               -> TExpr Word8
    TExprString :: String                              -> TExpr (Ptr Word8)
    -- ^ use createStringNul llvm instruction to store this
    -- and then getElementPtr to convert it into Ptr Word8
    TExprVar    :: TVariable a                         -> TExpr a
    TExprFun    :: LIde        -> TType a  -> [LAExpr] -> TExpr a
    TExprMinus  :: LTExpr a                            -> TExpr a
    TExprOp     :: LTExpr a    -> LTExpr a -> LTExpr a -> TExpr a

type LAExpr = Located AExpr

data AExpr = forall a . AExpr (TExpr a) (TType a)


type LTCond = Located TCond

data TCond
    = TCondTrue
    | TCondFalse
    | TCondNot LTCond
    | TCondOp LAExpr LOp LAExpr
    | TCondLog LTCond LOp LTCond


type LTVariable a = Located (TVariable a)

data TVariable a where
    TVar      :: LIde -> TType a                 -> TVariable a
    TVarArray :: LIde -> TType a -> LTExpr Int32 -> TVariable a

type LAVariable = Located AVariable

data AVariable = forall a . AVariable (TVariable a) (TType a)


type LTType a = Located (TType a)

data TType a where
    TTypeInt   :: TType Int32
    TTypeChar  :: TType Word8
    TTypeProc  :: TType ()
    TTypeArray :: TType a     -> TType (Ptr a)

{-

data TExpr a where
    TExprInt    :: Int32   -> TExpr Int32
    TExprChar   :: Word8   -> TExpr Word8
    TExprString :: String  -> TExpr (Ptr Word8)
    TExprVal    :: Ide     -> TType a       -> TExpr a
    TExprValArr :: Ide     -> TType (Ptr a) -> TExpr Int32 -> TExpr a
    TExprFun    :: Ide     -> TType a       -> [ATExpr]    -> TExpr a
    TExprSign   :: Op      -> TExpr a       -> TExpr a
    TExprOp     :: TExpr a -> Op            -> TExpr a     -> TExpr a

data ATExpr = forall a . (TExpr a) ::: (TType a)



data Equal a b where
    Eq :: Equal a a

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

extractATExpr :: (Type a) => ATExpr -> TExpr a
extractATExpr aexpr =
    case extract theType aexpr of
         Just x  -> x
         Nothing -> error "in extractATExpr"
    where extract :: TType a -> ATExpr -> Maybe (TExpr a)
          extract s (e ::: t) = do
              Eq <- test s t
              return e

-}
