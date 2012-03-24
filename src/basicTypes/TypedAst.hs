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
    TDefFun  :: (IsFirstClass a, Type a) =>
             LIde -> TType a -> [Either ADefFun ADefVar] -> TStmt -> TDefFun (IO a)
    TDefPar  :: (IsFirstClass a, IsFunction b, Type a, Type b) =>
             LIde -> TType a -> TDefFun b -> TDefFun (a->b)
    -- after LambdaLift we should have this instead of TDefFun
    TDefFunL :: (IsFirstClass a, Type a) =>
             LIde -> TType a -> [ADefVar] -> TStmt -> TDefFun (IO a)

data ADefFun = forall a. (IsFunction a, Type a) => ADefFun (TDefFun a) (TType a)

-- ---------------------------
data TDefVar a where
    TDefVar :: (IsFirstClass a, Type a) => LIde -> TType a -> TDefVar a
    TDefArr :: (IsFirstClass a, Type a) => TDefVar a -> Int32 -> TDefVar (Ptr a)

data ADefVar = forall a. (IsFirstClass a, Type a) => ADefVar (TDefVar a) (TType a)

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
    TExprVar    :: (IsFirstClass a, Type a) => TVariable a -> TExpr a
    TExprFun    :: (IsFirstClass a, Type a) => TFuncCall (IO a) -> TExpr a
    TExprMinus  :: TExpr Int32 -> TExpr Int32
    TExprOp     :: (IsFirstClass a, Type a) =>
                TExpr a -> Op -> TExpr a -> TExpr a

data AExpr = forall a. (IsFirstClass a, Type a) => AExpr (TExpr a) (TType a)

-- ---------------------------
data TCond a where
    TCondTrue  :: TCond Bool
    TCondFalse :: TCond Bool
    TCondNot   :: TCond Bool -> TCond Bool
    TCondOp    :: (IsFirstClass a, Type a) =>
               TExpr a -> Op -> TExpr a -> TCond Bool
    TCondLog   :: TCond Bool -> Op -> TCond Bool -> TCond Bool

data ACond = ACond (TCond Bool)

-- ---------------------------
data TVariable a where
    TVar      :: (IsFirstClass a, Type a) => Ide -> TType a -> TVariable a
    TVarArray :: (IsFirstClass a, Type a) => TVariable (Ptr a) -> TExpr Int32 -> TVariable a
    -- pointer to one variable
    TVarPtr   :: (IsFirstClass a, Type a) => TVariable a -> TVariable (Ptr a)

data AVariable = forall a. (IsFirstClass a, Type a) => AVariable (TVariable a) (TType a)

-- ---------------------------
data TType a where
    TTypeInt     :: TType Int32
    TTypeChar    :: TType Word8
    TTypeProc    :: TType ()
    TTypePtr     :: (IsFirstClass a, Type a) => TType a -> TType (Ptr a)
    TTypeUnknown :: TType ()
    TTypeFunc    :: (IsFirstClass a, IsFunction b, Type a, Type b) =>
                 TType a -> TType b -> TType (a->b)
    TTypeRetIO   :: (IsFirstClass a, Type a) => TType a -> TType (IO a)
    TTypeArray   :: (IsFirstClass a, Type a) => TType a -> Int32 -> TType (Ptr a)

data AType = forall a. (IsFirstClass a, Type a) => AType (TType a)

data ATypeF = forall a. (IsFunction a, Type a) => ATypeF (TType a)

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
    show (AType (TTypePtr t)) = showType t ++ " " ++ showArray (TTypePtr t)
    show (AType TTypeUnknown) = "unknown"
    show (AType (TTypeArray a s)) =
            showType a ++ " " ++ showArray (TTypeArray a s)
    show (AType _)            = panic "TypedAst.show got unexpected input"

showType :: (IsFirstClass a, Type a) => TType a -> String
showType (TTypeArray t _) = showType t
showType (TTypePtr t) = showType t
showType t = show $ AType t

showArray :: (IsFirstClass a, Type a) => TType a -> String
showArray (TTypeArray a s) =
    "[" ++ show s ++ "]" ++ showArray a
showArray (TTypePtr a) =
    "(*)" ++ showArray a
showArray _ = ""

-- ---------------------------
data TFuncCall a where
    TFuncCall  :: (IsFunction a, Type a) => LIde -> TType a -> TFuncCall a
    TParamCall :: (IsFirstClass a, IsFunction b, Type a, Type b) =>
               TExpr a -> TFuncCall (a->b) -> TFuncCall b

data AFuncCall = forall a. (IsFunction a, Type a) => AFuncCall (TFuncCall a) (TType a)


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


-- -------------------------------------------------------------------
-- To be able to extract TTypes from our DataTypes
class Type a where
    theType :: TType a

instance Type Int32 where
    theType = TTypeInt
instance Type Word8 where
    theType = TTypeChar
instance Type () where
    theType = TTypeProc
instance (Type a, IsFirstClass a) => Type (Ptr a) where
    theType = TTypePtr theType
instance (Type a, Type b, IsFirstClass a, IsFunction b) => Type (a->b) where
    theType = TTypeFunc theType theType
instance (Type a, IsFirstClass a) => Type (IO a) where
        theType = TTypeRetIO theType
