--------------------------------------------------------------------------------
-- (c) Tsitsimpis Ilias, 2011
--
-- Untyped Abstract Syntax Tree for the Alan Language
-- This is our parser's output
--
-- For each type we define an appropriate dump function
--------------------------------------------------------------------------------

module UnTypedAst (
    -- * DataTypes that don't contain a type
    LIde, Ide, LOp, Op(..), Mode(..),

    -- * DataTypes that need to be type checked
    UDef(..), UParam(..), UStmt(..), UExpr(..),
    UCond(..), UVariable(..), UType(..), UFuncCall(..),

    -- * Dump UnTypedAst
    UAst, dumpedUAst
  ) where

import SrcLoc
import {-# SOURCE #-} Outputable (panic)


-- ---------------------------
--
type UAst = UDef

dumpedUAst :: UAst -> String
dumpedUAst = dumpUDef 0

-- Used to indent the code when dumping ast
indent :: Int -> String
indent ind = take (2*ind) $ cycle " "

-- -------------------------------------------------------------------
-- This datatypes don't contain a type

type LIde = Located Ide

type Ide = String

dumpIde :: Ide -> String
dumpIde = id

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

instance Show Op where
    show = dumpOp

dumpOp :: Op -> String
dumpOp OpPlus     = "+"
dumpOp OpMinus    = "-"
dumpOp OpTimes    = "*"
dumpOp OpDiv      = "/"
dumpOp OpMod      = "%"
dumpOp OpAnd      = "&"
dumpOp OpOr       = "|"
dumpOp OpEqual    = "=="
dumpOp OpNotEqual = "!="
dumpOp OpLT       = "<"
dumpOp OpGT       = ">"
dumpOp OpLE       = "<="
dumpOp OpGE       = ">="

-- ---------------------------
data Mode
    = ModeByVal
    | ModeByRef
  deriving Eq

dumpMode :: Mode -> String
dumpMode ModeByVal = ""
dumpMode ModeByRef = "reference"


-- -------------------------------------------------------------------
-- This datatypes need to be type checked

type LUDef = Located UDef

data UDef
    = UDefFun LIde [LUParam] LUType [LUDef] LUStmt
    | UDefVar LIde LUType
    | UDefArr LUDef (Located Integer)

dumpUDef :: Int -> UDef -> String
-- UDefFun
dumpUDef ind (UDefFun lide luparams lutype ludefs lustmt) =
    indent ind ++ dumpIde (unLoc lide) ++ "(" ++ dumpLUParams luparams ++
        ") : " ++ dumpUType (unLoc lutype) ++
        "\n" ++ dumpLUDefs (ind+1) ludefs ++
        dumpComp (ind+1) (unLoc lide) (unLoc lustmt)
-- UDefVar
dumpUDef ind (UDefVar lide lutype) =
    indent ind ++ dumpIde (unLoc lide) ++ " : " ++
        dumpUType (unLoc lutype) ++ ";"
-- UDefArr
dumpUDef ind (UDefArr ludef lsize) =
    indent ind ++ init (dumpUDef 0 (unLoc ludef)) ++
        "[" ++ show (unLoc lsize) ++ "];"

dumpLUDefs :: Int -> [LUDef] -> String
dumpLUDefs ind ludefs =
    foldl (\buf t -> buf ++ dumpUDef ind (unLoc t) ++ "\n") "" ludefs

-- ---------------------------
type LUParam = Located UParam

data UParam
    = UParam LIde Mode LUType

instance Show UParam where
    show = dumpUParam

dumpUParam :: UParam -> String
dumpUParam (UParam lide mode lutype) =
    dumpIde (unLoc lide) ++ " : " ++ dumpMode mode ++ " " ++
        dumpUType (unLoc lutype)

dumpLUParams :: [LUParam] -> String
dumpLUParams [] = ""
dumpLUParams (luparam:luparams) =
    dumpUParam (unLoc luparam) ++
        (foldl (\buf t -> buf ++ ", " ++ dumpUParam (unLoc t)) "" luparams)

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

instance Show UStmt where
    show = dumpUStmt 0

dumpUStmt :: Int -> UStmt -> String
-- UStmtNothing
dumpUStmt ind UStmtNothing = indent ind ++ ";"
-- UStmtAssign
dumpUStmt ind (UStmtAssign luvar luexpr) =
    indent ind ++ dumpUVariable (unLoc luvar) ++
        " = " ++ dumpUExpr (unLoc luexpr) ++ ";"
-- UStmtCompound
dumpUStmt ind (UStmtCompound lustmts) =
    indent ind ++ "{\n" ++ dumpLUStmts (ind+1) lustmts ++
        indent ind ++ "}"
-- UStmtFun
dumpUStmt ind (UStmtFun ufunc) =
    indent ind ++ dumpUFuncCall ufunc ++ ";"
-- UStmtIf
dumpUStmt ind (UStmtIf lucond ifstmt m_elsestmt) =
    indent ind ++ "if(" ++ dumpUCond (unLoc lucond) ++
        ")" ++ dumpComp (ind+1) "" (unLoc ifstmt) ++
        case m_elsestmt of
             Just elsestmt ->
                 " else" ++ dumpComp (ind+1) "" (unLoc elsestmt)
             Nothing -> ""
-- UStmtWhile
dumpUStmt ind (UStmtWhile lucond lustmt) =
    indent ind ++ "while(" ++ dumpUCond (unLoc lucond) ++
        ")" ++ dumpComp (ind+1) "" (unLoc lustmt)
-- UStmtReturn
dumpUStmt ind (UStmtReturn m_luexpr) =
    indent ind ++ "return " ++
        case m_luexpr of
             Just luexpr -> dumpUExpr (unLoc luexpr) ++ ";"
             Nothing     -> ";"

dumpLUStmts :: Int -> [LUStmt] -> String
dumpLUStmts ind lustmts =
    foldl (\buf t -> buf ++ dumpUStmt ind (unLoc t) ++ "\n") "" lustmts

dumpComp :: Int -> String -> UStmt -> String
dumpComp ind "" (UStmtCompound lustmts) =
    " {\n" ++ dumpLUStmts ind lustmts ++ indent (ind-1) ++ "}"
dumpComp ind "" ustmt =
    "\n" ++ dumpUStmt ind ustmt
dumpComp ind fun (UStmtCompound lustmts) =
    indent (ind-1) ++ "{ -- " ++ fun ++ "\n" ++ dumpLUStmts ind lustmts ++
        indent (ind-1) ++ "} -- " ++ fun
dumpComp _ _ _ = panic "UnTypedAst.dumpComp got unexpected input"

-- ---------------------------
type LUExpr = Located UExpr

data UExpr
    = UExprInt Integer
    | UExprChar Char
    | UExprString String
    | UExprVar UVariable
    | UExprFun UFuncCall
    | UExprMinus LUExpr
    | UExprOp LUExpr LOp LUExpr

instance Show UExpr where
    show = dumpUExpr

dumpUExpr :: UExpr -> String
dumpUExpr (UExprInt i)    = show i
dumpUExpr (UExprChar c)   = show c
dumpUExpr (UExprString s) = "\"" ++ escape s ++ "\""
dumpUExpr (UExprVar v)    = dumpUVariable v
dumpUExpr (UExprFun f)    = dumpUFuncCall f
dumpUExpr (UExprMinus e)  = "-" ++ dumpUExpr (unLoc e)
dumpUExpr (UExprOp a o b) =
    dumpUExpr (unLoc a) ++ " " ++ dumpOp (unLoc o) ++ " " ++ dumpUExpr (unLoc b)

dumpLUExprs :: [LUExpr] -> String
dumpLUExprs [] = ""
dumpLUExprs (luexpr:luexprs) =
    dumpUExpr (unLoc luexpr) ++
        (foldl (\buf t -> buf ++ ", " ++ dumpUExpr (unLoc t)) "" luexprs)

escape :: String -> String
escape ('\n' : s) = '\\' : 'n'  : escape s
escape ('\t' : s) = '\\' : 't'  : escape s
escape ('\r' : s) = '\\' : 'r'  : escape s
escape ('\0' : s) = '\\' : '0'  : escape s
escape ('\\' : s) = '\\' : '\\' : escape s
escape ('\'' : s) = '\\' : '\'' : escape s
escape ('\"' : s) = '\\' : '\"' : escape s
escape ( x   : s) = x : escape s
escape [] = []

-- ---------------------------
type LUCond = Located UCond

data UCond
    = UCondTrue
    | UCondFalse
    | UCondNot LUCond
    | UCondOp LUExpr LOp LUExpr
    | UCondLog LUCond LOp LUCond

instance Show UCond where
    show = dumpUCond

dumpUCond :: UCond -> String
dumpUCond UCondTrue        = "true"
dumpUCond UCondFalse       = "false"
dumpUCond (UCondNot c)     = "!" ++ dumpUCond (unLoc c)
dumpUCond (UCondOp a o b)  =
    dumpUExpr (unLoc a) ++ " " ++ dumpOp (unLoc o) ++ " " ++ dumpUExpr (unLoc b)
dumpUCond (UCondLog a o b) =
    dumpUCond (unLoc a) ++ " " ++ dumpOp (unLoc o) ++ " " ++ dumpUCond (unLoc b)

-- ---------------------------
type LUVariable = Located UVariable

data UVariable
    = UVar Ide
    | UVarArray LUVariable LUExpr

instance Show UVariable where
    show = dumpUVariable

dumpUVariable :: UVariable -> String
dumpUVariable (UVar ide)      = dumpIde ide
dumpUVariable (UVarArray v e) =
    dumpUVariable (unLoc v) ++ "[" ++ dumpUExpr (unLoc e) ++ "]"

-- ---------------------------
type LUType = Located UType

data UType
    = UTypeInt
    | UTypeChar
    | UTypeProc
    | UTypePtr UType

dumpUType :: UType -> String
dumpUType UTypeInt = "int"
dumpUType UTypeChar = "byte"
dumpUType UTypeProc = "proc"
dumpUType (UTypePtr utype) =
    dumpUType utype ++ "[]"

-- ---------------------------
data UFuncCall = UFuncCall LIde [LUExpr]

instance Show UFuncCall where
    show = dumpUFuncCall

dumpUFuncCall :: UFuncCall -> String
dumpUFuncCall (UFuncCall lide luexprs) =
    dumpIde (unLoc lide) ++ "(" ++ dumpLUExprs luexprs ++ ")"
