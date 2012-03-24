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

import UnTypedAst (LIde, Ide, Op)
import Outputable (panic)

import Data.Int
import Data.Word
import Foreign.Ptr
import Control.Monad
import LLVM.Core (IsFunction, IsFirstClass)


-- ---------------------------
--
type TAst = ADefFun

type ADef = Either ADefFun ADefVar

-- ---------------------------
data TDefFun a where
    TDefFun  :: (IsFirstClass a) =>
             LIde -> TType a -> [Either ADefFun ADefVar] -> TStmt -> TDefFun (IO a)
    TDefPar  :: (IsFirstClass a, IsFunction b) =>
             LIde -> TType a -> TDefFun b -> TDefFun (a->b)
    -- after LambdaLift we should have this instead of TDefFun
    TDefFunL :: (IsFirstClass a) =>
             LIde -> TType a -> [ADefVar] -> TStmt -> TDefFun (IO a)

data ADefFun = forall a. (IsFunction a) => ADefFun (TDefFun a) (TType a)

-- ---------------------------
data TDefVar a where
    TDefVar :: (IsFirstClass a) => LIde -> TType a -> TDefVar a
    TDefArr :: (IsFirstClass a) => TDefVar a -> Word32 -> TDefVar (Ptr a)

data ADefVar = forall a. (IsFirstClass a) => ADefVar (TDefVar a) (TType a)

-- ---------------------------
data TStmt
    = TStmtNothing
    | TStmtAssign AVariable AExpr
    | TStmtCompound [TStmt]
    | TStmtFun AFuncCall
    | TStmtIf ACond TStmt (Maybe TStmt)
    | TStmtWhile ACond TStmt
    | TStmtReturn (Maybe AExpr)

-- ---------------------------
data TExpr a where
    TExprInt    :: Int32   -> TExpr Int32
    TExprChar   :: Word8   -> TExpr Word8
    TExprString :: String  -> TExpr (Ptr Word8)
    TExprVar    :: (IsFirstClass a) => TVariable a -> TExpr a
    TExprFun    :: (IsFirstClass a) => TFuncCall (IO a) -> TExpr a
    TExprMinus  :: TExpr Int32 -> TExpr Int32
    TExprIntOp  :: TExpr Int32 -> Op -> TExpr Int32 -> TExpr Int32
    TExprChrOp  :: TExpr Word8 -> Op -> TExpr Word8 -> TExpr Word8

data AExpr = forall a. (IsFirstClass a) => AExpr (TExpr a) (TType a)

-- ---------------------------
data TCond a where
    TCondTrue  :: TCond Bool
    TCondFalse :: TCond Bool
    TCondNot   :: TCond Bool -> TCond Bool
    TCondIntOp :: TExpr Int32 -> Op -> TExpr Int32 -> TCond Bool
    TCondChrOp :: TExpr Word8 -> Op -> TExpr Word8 -> TCond Bool
    TCondLog   :: TCond Bool -> Op -> TCond Bool -> TCond Bool

data ACond = ACond (TCond Bool)

-- ---------------------------
data TVariable a where
    TVar      :: (IsFirstClass a) => Ide -> TType a -> TVariable a
    TVarArray :: (IsFirstClass a) => TVariable (Ptr a) -> TExpr Int32 -> TVariable a
    -- pointer to one variable
    TVarPtr   :: (IsFirstClass a) => TVariable a -> TVariable (Ptr a)

data AVariable = forall a. (IsFirstClass a) => AVariable (TVariable a) (TType a)

-- ---------------------------
data TType a where
    TTypeInt     :: TType Int32
    TTypeChar    :: TType Word8
    TTypeProc    :: TType ()
    TTypePtr     :: (IsFirstClass a) => TType a -> TType (Ptr a)
    TTypeUnknown :: TType ()
    TTypeFunc    :: (IsFirstClass a, IsFunction b) => TType a -> TType b -> TType (a->b)
    TTypeRetIO   :: (IsFirstClass a) => TType a -> TType (IO a)
    TTypeArray   :: (IsFirstClass a) => TType a -> Word32 -> TType (Ptr a)

data AType = forall a. (IsFirstClass a) => AType (TType a)

data ATypeF = forall a. (IsFunction a) => ATypeF (TType a)

instance Eq AType where
    (AType TTypeUnknown)      == (AType TTypeUnknown)      = True
    (AType TTypeInt)          == (AType TTypeInt)          = True
    (AType TTypeChar)         == (AType TTypeChar)         = True
    (AType TTypeProc)         == (AType TTypeProc)         = True
    (AType (TTypePtr a))      == (AType (TTypePtr b))      = AType a == AType b
    (AType (TTypeArray a sa)) == (AType (TTypeArray b sb)) =
        (AType a == AType b) && (sa == sb)
    (AType (TTypeArray a _))  == (AType (TTypePtr b))      = AType a == AType b
    (AType (TTypePtr a))      == (AType (TTypeArray b _))  = AType a == AType b
    (AType _) == (AType _) = False

instance Eq ATypeF where
    (ATypeF (TTypeRetIO a)) == (ATypeF (TTypeRetIO b)) = AType a == AType b
    (ATypeF _) == (ATypeF _) = False

instance Show AType where
    show (AType TTypeInt)     = "int"
    show (AType TTypeChar)    = "byte"
    show (AType TTypeProc)    = "proc"
    show (AType (TTypePtr t)) = "array of " ++ show (AType t)
    show (AType TTypeUnknown) = "unknown"
    show (AType (TTypeArray a s)) =
        show (AType a) ++ "[" ++ show s ++ "]"
    show (AType _)            = panic "TypedAst.show got unexpected input"

-- ---------------------------
data TFuncCall a where
    TFuncCall  :: (IsFunction a) => LIde -> TType a -> TFuncCall a
    TParamCall :: (IsFirstClass a, IsFunction b) =>
               TExpr a -> TFuncCall (a->b) -> TFuncCall b

data AFuncCall = forall a. (IsFunction a) => AFuncCall (TFuncCall a) (TType a)


-- -------------------------------------------------------------------
-- We need to do the equality test so that it reflects the equality
-- on the type level. There's a standard trick for this.
-- If you ever have a value (which must be Eq) of type
-- Equal foo bar then the type checker will know that foo and
-- bar are actually the same type.
data Equal a b where
    Eq :: Equal a a

test :: TType a -> TType b -> Maybe (Equal a b)
test TTypeInt      TTypeInt     = return Eq
test TTypeChar     TTypeChar    = return Eq
test TTypeProc     TTypeProc    = return Eq
test TTypeUnknown  TTypeUnknown = return Eq
test (TTypePtr a)  (TTypePtr b) = do
    Eq <- test a b
    return Eq
test (TTypeArray a _) (TTypePtr b) = do
    Eq <- test a b
    return Eq
test (TTypePtr a) (TTypeArray b _) = do
    Eq <- test a b
    return Eq
test (TTypeArray a sa) (TTypeArray b sb) = do
    if sa == sb
       then do Eq <- test a b; return Eq
       else mzero
test (TTypeRetIO a) (TTypeRetIO b) = do
    Eq <- test a b
    return Eq
test _ _ = mzero
