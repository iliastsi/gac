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
-- DataType containing the basic blocks of the UnTyped AST
-- (for printing purposes, used in TypeError of MsgCode)

data UAst
    = UAstS UStmt
    | UAstE UExpr
    | UAstC UCond
    | UAstV UVariable
    | UAstF UFuncCall

instance Show UAst where
    show (UAstS s) = show s
    show (UAstE e) = show e
    show (UAstC c) = show c
    show (UAstV v) = show v
    show (UAstF f) = show f

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
  deriving Eq

instance Show Mode where
    show ModeByVal = ""
    show ModeByRef = "reference"


-- -------------------------------------------------------------------
-- This datatypes need to be type checked

type LUDef = Located UDef

data UDef
    = UDefFun LIde [LUParam] LUType [LUDef] LUStmt
    | UDefVar LIde LUType
  deriving Eq

-- ---------------------------
type LUParam = Located UParam

data UParam
    = UParam LIde Mode LUType
  deriving Eq

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
  deriving Eq

instance Show UStmt where
    show (UStmtAssign lvar lexpr) =
                show (unLoc lvar) ++ " = " ++ show (unLoc lexpr) ++ ";"
    show (UStmtFun f)           = show f
    show (UStmtReturn Nothing)  = "return;"
    show (UStmtReturn (Just e)) = "return " ++ show (unLoc e) ++ ";"


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
    show (UExprString s) = show s
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
  deriving Eq

instance Show UCond where
    show UCondTrue  = "true"
    show UCondFalse = "false"
    show (UCondNot lcond) = "!" ++ show (unLoc lcond)
    show (UCondOp a o b) =
        show (unLoc a) ++ " " ++ show (unLoc o) ++ " " ++ show (unLoc b)
    show (UCondLog a o b) =
        show (unLoc a) ++ " " ++ show (unLoc o) ++ " " ++ show (unLoc b)

-- ---------------------------
type LUVariable = Located UVariable

data UVariable
    = UVar Ide
    | UVarArray LIde LUExpr
  deriving Eq

instance Show UVariable where
    show (UVar i)        = show i
    show (UVarArray i e) =
        show (unLoc i) ++ "[" ++ show (unLoc e) ++ "]"

-- ---------------------------
type LUType = Located UType

data UType
    = UTypeInt
    | UTypeChar
    | UTypeProc
    | UTypeArray (Int, UType)
    | UTypeUnknown -- for type checking
  deriving Eq

-- ---------------------------
data UFuncCall = UFuncCall LIde [LUExpr]
  deriving Eq

instance Show UFuncCall where
    show (UFuncCall i [])     = show (unLoc i) ++ "()"
    show (UFuncCall i (e:es)) =
        show (unLoc i) ++ "(" ++ show (unLoc e) ++
            (foldl (\str t -> str ++ ", " ++ show (unLoc t)) "" es) ++ ")"
