--------------------------------------------------------------------------------
-- (c) Tsitsimpis Ilias, 2011-2012
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

import UnTypedAst (LIde, Ide, LOp)
import SrcLoc
import Outputable (panic)

import Data.Int
import Data.Word
import Foreign.Ptr
import Control.Monad


-- ---------------------------
--
type TAst = ADef

-- ---------------------------
type LTDef a = Located (TDef a)

data TDef a where
      -- functions
    TDefFun :: (Type a) => LIde -> LTType a -> [LADef] -> LTStmt -> TDef a
    TDefPar :: (Type a, Type b) => LIde -> LTType a -> LTDef b -> TDef (a -> b)
      -- variables
    TDefVar :: (Type a) => LIde    -> LTType a       -> TDef a
    TDefArr :: (Type a) => LTDef a -> Located Word32 -> TDef (Ptr a)

type LADef = Located ADef

data ADef = forall a . Type a => ADef (TDef a) (TType a)

-- ---------------------------
type LTStmt = Located TStmt

data TStmt
    = TStmtNothing
    | TStmtAssign LAVariable LAExpr
    | TStmtCompound [LTStmt]
    | TStmtFun LAFuncCall
    | TStmtIf LTCond LTStmt (Maybe LTStmt)
    | TStmtWhile LTCond LTStmt
    | TStmtReturn (Maybe LAExpr)

-- ---------------------------
type LTExpr a = Located (TExpr a)

data TExpr a where
    TExprInt    :: Int32   -> TExpr Int32
    TExprChar   :: Word8   -> TExpr Word8
    TExprString :: String  -> TExpr (Ptr Word8)
    TExprVar    :: (Type a) => TVariable a -> TExpr a
    TExprFun    :: (Type a) => TFuncCall a -> TExpr a
    TExprMinus  :: (Type a) => LTExpr a    -> TExpr a
    TExprOp     :: (Type a) => LTExpr a -> LOp -> LTExpr a -> TExpr a

type LAExpr = Located AExpr

data AExpr = forall a . Type a => AExpr (TExpr a) (TType a)

-- ---------------------------
type LTCond = Located TCond

data TCond
    = TCondTrue
    | TCondFalse
    | TCondNot LTCond
    | TCondOp LAExpr LOp LAExpr
    | TCondLog LTCond LOp LTCond

-- ---------------------------
type LTVariable a = Located (TVariable a)

data TVariable a where
    TVar      :: (Type a) => Ide                -> TType a      -> TVariable a
    TVarArray :: (Type a) => LTVariable (Ptr a) -> LTExpr Int32 -> TVariable a
    -- pointer to one variable
    TVarPtr   :: (Type a) => TVariable a -> TVariable (Ptr a)

type LAVariable = Located AVariable

data AVariable = forall a . Type a => AVariable (TVariable a) (TType a)

-- ---------------------------
type LTType a = Located (TType a)

data TType a where
    TTypeInt        :: TType Int32
    TTypeChar       :: TType Word8
    TTypeProc       :: TType ()
    TTypePtr        :: (Type a) => TType a -> TType (Ptr a)
    TTypeUnknown    :: TType ()
    TTypeFunc       :: (Type a, Type b) => TType a -> TType b -> TType (a -> b)

type LAType = Located AType

data AType = forall a . Type a => AType (TType a)

instance Eq AType where
    (AType TTypeUnknown) == (AType TTypeUnknown) = True
    (AType TTypeInt)     == (AType TTypeInt)     = True
    (AType TTypeChar)    == (AType TTypeChar)    = True
    (AType TTypeProc)    == (AType TTypeProc)    = True
    (AType (TTypePtr a)) == (AType (TTypePtr b)) = AType a == AType b
    (AType _)            == (AType _)            = False

instance Show AType where
    show (AType TTypeInt)     = "int"
    show (AType TTypeChar)    = "byte"
    show (AType TTypeProc)    = "proc"
    show (AType (TTypePtr t)) = "array of " ++ show (AType t)
    show (AType TTypeUnknown) = "unknown"
    show (AType TTypeFunc {}) = panic "TypedAst.show cannot handle (AType TTypeFunc)"

-- ---------------------------
type LTFuncCall a = Located (TFuncCall a)

data TFuncCall a where
    TFuncCall  :: (Type a) => LIde -> TType a -> TFuncCall a
    TParamCall :: (Type a, Type b) => TExpr a -> LTFuncCall (a->b) -> TFuncCall b

type LAFuncCall = Located AFuncCall

data AFuncCall = forall a . Type a => AFuncCall (TFuncCall a) (TType a)


-- -------------------------------------------------------------------
-- We need to do the equality test so that it reflects the equality
-- on the type level. There's a standard trick for this.
-- If you ever have a value (which must be Eq) of type
-- Equal foo bar then the type checker will know that foo and
-- bar are actually the same type.
data Equal a b where
    Eq :: Equal a a

test :: (Type a, Type b) => TType a -> TType b -> Maybe (Equal a b)
test TTypeInt      TTypeInt     = return Eq
test TTypeChar     TTypeChar    = return Eq
test TTypeProc     TTypeProc    = return Eq
test TTypeUnknown  TTypeUnknown = return Eq
test (TTypePtr a)  (TTypePtr b) = do
    Eq <- test a b
    return Eq
test _ _ = mzero


-- -------------------------------------------------------------------
-- To be able to extract a TDef from a ADef we need some small utilties

class Type a where
    theType :: TType a
instance Type Int32 where
    theType = TTypeInt
instance Type Word8 where
    theType = TTypeChar
instance Type () where
    theType = TTypeProc
instance (Type a) => Type (Ptr a) where
    theType = TTypePtr theType
instance (Type a, Type b) => Type (a->b) where
    theType = TTypeFunc theType theType

extractTDef :: (Type a) => ADef -> TDef a
extractTDef adef =
    case extract theType adef of
         Just x  -> x
         Nothing -> panic "extract in TypedAst.extractTDef returned Nothing"
    where extract :: (Type a) => TType a -> ADef -> Maybe (TDef a)
          extract s (ADef e t) = do
              Eq <- test s t
              return e

extractTExpr :: (Type a) => AExpr -> TExpr a
extractTExpr aexpr =
    case extract theType aexpr of
         Just x  -> x
         Nothing -> panic "extract in TypedAst.extractTExpr returned Nothing"
    where extract :: (Type a) => TType a -> AExpr -> Maybe (TExpr a)
          extract s (AExpr e t) = do
              Eq <- test s t
              return e

extractTVariable :: (Type a) => AVariable -> TVariable a
extractTVariable avar =
    case extract theType avar of
         Just x  -> x
         Nothing -> panic "extract in TypedAst.extractTVariable returned Nothing"
    where extract :: (Type a) => TType a -> AVariable -> Maybe (TVariable a)
          extract s (AVariable e t) = do
              Eq <- test s t
              return e
