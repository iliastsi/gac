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

import UnTypedAst (UType(..), LIde, Ide, LOp, Mode)
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

-- ---------------------------
type LTStmt = Located TStmt

data TStmt
    = TStmtNothing
    | TStmtAssign LAVariable LAExpr
    | TStmtCompound [LTStmt]
    | TStmtFun LIde [LAExpr]
    | TStmtIf LTCond LTStmt (Maybe LTStmt)
    | TStmtWhile LTCond LTStmt
    | TStmtReturn (Maybe LAExpr )

-- ---------------------------
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
    TVar      :: Ide  -> TType a                 -> TVariable a
    TVarArray :: LIde -> TType a -> LTExpr Int32 -> TVariable a

type LAVariable = Located AVariable

data AVariable = forall a . AVariable (TVariable a) (TType a)

-- ---------------------------
type LTType a = Located (TType a)

data TType a where
    TTypeInt        :: TType Int32
    TTypeChar       :: TType Word8
    TTypeProc       :: TType ()
    TTypeArray      :: Int -> TType a -> TType (Ptr a)
    TTypeUnknown    :: TType ()

type LAType = Located AType

data AType = forall a . AType (TType a)

instance Eq AType where
    (AType TTypeUnknown) == (AType _)            = True
    (AType _)            == (AType TTypeUnknown) = True
    (AType a)            == (AType b) =
        case test a b of
             Just Eq -> True
             Nothing -> False


-- -------------------------------------------------------------------
-- We need to do the equality test so that it reflects the equality
-- on the type level. There's a standard trick for this.
-- If you ever have a value (which must be Eq) of type
-- Equal foo bar then the type checker will know that foo and
-- bar are actually the same type.
data Equal a b where
    Eq :: Equal a a

test :: TType a -> TType b -> Maybe (Equal a b)
test TTypeInt          TTypeInt         = return Eq
test TTypeChar         TTypeChar        = return Eq
test TTypeProc         TTypeProc        = return Eq
test (TTypeArray _ a)  (TTypeArray _ b) = do
    Eq <- test a b
    return Eq
test _ _ = mzero


-- -------------------------------------------------------------------
-- To be able to extract a TDef from a ADef we need some small utilties
--

class Type a where
    theType :: TType a
instance Type Int32 where
    theType = TTypeInt
instance Type Word8 where
    theType = TTypeChar
instance Type () where
    theType = TTypeProc
instance (Type a) => Type (Ptr a) where
    theType = TTypeArray 0 theType

extractTDef :: (Type a) => ADef -> TDef a
extractTDef adef =
    case extract theType adef of
         Just x  -> x
         Nothing -> error "in extractTDef"
    where extract :: TType a -> ADef -> Maybe (TDef a)
          extract s (ADef e t) = do
              Eq <- test s t
              return e

extractTExpr :: (Type a) => AExpr -> TExpr a
extractTExpr aexpr =
    case extract theType aexpr of
         Just x  -> x
         Nothing -> error "in extractTExpr"
    where extract :: TType a -> AExpr -> Maybe (TExpr a)
          extract s (AExpr e t) = do
              Eq <- test s t
              return e

extractTVariable :: (Type a) => AVariable -> TVariable a
extractTVariable avar =
    case extract theType avar of
         Just x  -> x
         Nothing -> error "in extractTVariable"
    where extract :: TType a -> AVariable -> Maybe (TVariable a)
          extract s (AVariable e t) = do
              Eq <- test s t
              return e


-- -------------------------------------------------------------------
-- Convert between UType and TType

matchType :: UType -> TType a -> Bool
matchType UTypeInt              TTypeInt         = True
matchType UTypeChar             TTypeChar        = True
matchType UTypeProc             TTypeProc        = True
matchType (UTypeArray (_,ut))  (TTypeArray _ tt) = matchType ut tt
matchType _  _  = False

fromUType :: UType -> AType
fromUType UTypeInt  = AType TTypeInt
fromUType UTypeChar = AType TTypeChar
fromUType UTypeProc = AType TTypeProc
fromUType (UTypeArray (s,t)) =
    case fromUType t of
         AType tt  -> AType (TTypeArray s tt)
fromUType UTypeUnknown = AType TTypeUnknown
