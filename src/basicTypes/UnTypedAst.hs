--------------------------------------------------------------------------------
-- (c) Tsitsimpis Ilias, 2011
--
-- Untyped Abstract Syntax Tree for the Alan Language
-- This is our parser's output
--
--------------------------------------------------------------------------------

module UnTypedAst where

import SrcLoc


-- -------------------------------------------------------------------
-- This datatypes don't contain a type

type LIde = Located Ide

type Ide = String

-- ---------------------------
type LOp = Located Op

data Op
    = OpPlus
    | OpMinus
    | OpTimes
    | OpDiv
    | OpMod
    | OpAnd
    | OpOr
    | OpEqual
    | OpNotEqual
    | OpLT
    | OpGT
    | OpLE
    | OpGE
  deriving Eq

instance Show Op where
    show OpPlus     = "+"
    show OpMinus    = "-"
    show OpTimes    = "*"
    show OpDiv      = "/"
    show OpMod      = "%"
    show OpAnd      = "&"
    show OpOr       = "|"
    show OpEqual    = "=="
    show OpNotEqual = "!="
    show OpLT       = "<"
    show OpGT       = ">"
    show OpLE       = "<="
    show OpGE       = ">="

-- ---------------------------
data Mode
    = ModeByVal
    | ModeByRef
  deriving (Eq, Show)


-- -------------------------------------------------------------------
-- This datatypes need to be type checked

type LUDef = Located UDef

data UDef
    = UDefFun LIde [LUDef] LUType [LUDef] LUStmt
    | UDefPar LIde Mode LUType
    | UDefVar LIde LUType
  deriving (Eq, Show)

-- ---------------------------
type LUStmt = Located UStmt

data UStmt
    = UStmtNothing
    | UStmtAssign LUVariable LUExpr
    | UStmtCompound [LUStmt]
    | UStmtFun UFuncCall
    | UStmtIf LUCond LUStmt (Maybe LUStmt)
    | UStmtWhile LUCond LUStmt
    | UStmtReturn (Maybe LUExpr)
  deriving (Eq, Show)

-- ---------------------------
type LUExpr = Located UExpr

data UExpr
    = UExprInt Int
    | UExprChar Char
    | UExprString String
    | UExprVar UVariable
    | UExprFun UFuncCall
    | UExprMinus LUExpr
    | UExprOp LUExpr LOp LUExpr
  deriving Eq

instance Show UExpr where
    show (UExprInt i)    = show i
    show (UExprChar c)   = show c
    show (UExprString s) = s
    show (UExprVar v)    = show v
    show (UExprFun f)    = show f
    show (UExprMinus e)  = "-" ++ show (unLoc e)
    show (UExprOp a o b) =
        show (unLoc a) ++ " " ++ show (unLoc o) ++ " " ++ show (unLoc b)

-- ---------------------------
type LUCond = Located UCond

data UCond
    = UCondTrue
    | UCondFalse
    | UCondNot LUCond
    | UCondOp LUExpr LOp LUExpr
    | UCondLog LUCond LOp LUCond
  deriving (Eq, Show)

-- ---------------------------
type LUVariable = Located UVariable

data UVariable
    = UVar Ide
    | UVarArray LIde LUExpr
  deriving Eq

instance Show UVariable where
    show (UVar i)        = i
    show (UVarArray i e) =
        (unLoc i) ++ "[" ++ show (unLoc e) ++ "]"

-- ---------------------------
type LUType = Located UType

data UType
    = UTypeInt
    | UTypeChar
    | UTypeProc
    | UTypeArray (Int, UType)
  deriving (Eq, Show)

-- ---------------------------
data UFuncCall = UFuncCall LIde [LUExpr]
  deriving Eq

instance Show UFuncCall where
    show (UFuncCall i [])     = (unLoc i) ++ "()"
    show (UFuncCall i (e:es)) =
        (unLoc i) ++ "(" ++ show (unLoc e) ++
            (foldl (\str t -> str ++ ", " ++ show (unLoc t)) "" es) ++ ")"
