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
    UAst, dumpUnTypedAst
  ) where

import SrcLoc


-- ---------------------------
type UAst = UDef

dumpUnTypedAst :: UDef -> String
dumpUnTypedAst = dumpUDef 0

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
    | UDefVar LIde (Located Int) LUType

dumpUDef :: Int -> UDef -> String
-- UDefFun
dumpUDef ind (UDefFun lide luparams lutype ludefs lustmt) =
    indent ind ++ dumpIde (unLoc lide) ++ "(" ++ dumpLUParams luparams ++
        ") : " ++ dumpUType (TypeEmpty) (unLoc lutype) ++
        "\n" ++ dumpLUDefs (ind+1) ludefs ++ "\n" ++
        dumpUStmt (ind+1) (unLoc lide) (unLoc lustmt)
-- UDefVar
dumpUDef ind (UDefVar lide lsize lutype) =
    indent ind ++ dumpIde (unLoc lide) ++ " : " ++
        dumpUType (TypeWith $ unLoc lsize) (unLoc lutype) ++ ";"

dumpLUDefs :: Int -> [LUDef] -> String
dumpLUDefs ind [] = ""
dumpLUDefs ind (ludef:ludefs) =
    dumpUDef ind (unLoc ludef) ++
        (foldl (\buf t -> buf ++ "\n" ++ dumpUDef ind (unLoc t)) "" ludefs)
            
-- ---------------------------
type LUParam = Located UParam

data UParam
    = UParam LIde Mode LUType

instance Show UParam where
    show = dumpUParam

dumpUParam :: UParam -> String
dumpUParam (UParam lide mode lutype) =
    dumpIde (unLoc lide) ++ " : " ++ dumpMode mode ++ " " ++ 
        dumpUType (TypeEmpty) (unLoc lutype)

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
    show = dumpUStmt 0 ""

dumpUStmt :: Int -> String -> UStmt -> String
-- UStmtNothing
dumpUStmt _ _ UStmtNothing = ";"
-- UStmtAssign
dumpUStmt ind _ (UStmtAssign luvar luexpr) =
    indent ind ++ dumpUVariable (unLoc luvar) ++
        " = " ++ dumpUExpr (unLoc luexpr) ++ ";"
-- UStmtCompound
dumpUStmt ind comment (UStmtCompound lustmts) =
    indent (ind-1) ++ "{ -- " ++ comment ++ "\n" ++
        dumpLUStmts ind lustmts ++ "\n" ++ indent (ind-1) ++
        "} -- " ++ comment
-- UStmtFun
dumpUStmt ind _ (UStmtFun ufunc) =
    indent ind ++ dumpUFuncCall ufunc ++ ";"
-- UStmtIf
dumpUStmt ind _ (UStmtIf lucond ifstmt m_elsestmt) =
    indent ind ++ "if(" ++ dumpUCond (unLoc lucond) ++
        ")\n" ++ dumpUStmt (ind+1) "if-clause" (unLoc ifstmt) ++
        case m_elsestmt of
             Just elsestmt ->
                 indent ind ++ "else" ++ "\n" ++
                     dumpUStmt (ind+1) "else-clause" (unLoc elsestmt)
             Nothing -> ""
-- UStmtWhile
dumpUStmt ind _ (UStmtWhile lucond lustmt) =
    indent ind ++ "while(" ++ dumpUCond (unLoc lucond) ++
        ")\n" ++ dumpUStmt (ind+1) "while-clause" (unLoc lustmt)
-- UStmtReturn
dumpUStmt ind _ (UStmtReturn m_luexpr) =
    indent ind ++ "return " ++
        case m_luexpr of
             Just luexpr -> dumpUExpr (unLoc luexpr) ++ ";"
             Nothing     -> ";"

dumpLUStmts :: Int -> [LUStmt] -> String
dumpLUStmts ind [] = ""
dumpLUStmts ind (lustmt:lustmts) =
    dumpUStmt ind "" (unLoc lustmt) ++
        (foldl (\buf t -> buf ++ "\n" ++ dumpUStmt ind "" (unLoc t)) "" lustmts)

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

instance Show UExpr where
    show = dumpUExpr

dumpUExpr :: UExpr -> String
dumpUExpr (UExprInt i)    = show i
dumpUExpr (UExprChar c)   = show c
dumpUExpr (UExprString s) = show s
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
    | UVarArray LIde LUExpr

instance Show UVariable where
    show = dumpUVariable

dumpUVariable :: UVariable -> String
dumpUVariable (UVar ide)      = dumpIde ide
dumpUVariable (UVarArray i e) =
    dumpIde (unLoc i) ++ "[" ++ dumpUExpr (unLoc e) ++ "]"

-- ---------------------------
type LUType = Located UType

data UType
    = UTypeInt
    | UTypeChar
    | UTypeProc
    | UTypePtr UType

dumpUType :: TypeShow -> UType -> String
dumpUType _ UTypeInt = "int"
dumpUType _ UTypeChar = "byte"
dumpUType _ UTypeProc = "proc"
dumpUType TypeEmpty    (UTypePtr utype) =
    dumpUType TypeEmpty utype ++ "[]"
dumpUType (TypeWith n) (UTypePtr utype) =
    dumpUType TypeEmpty utype ++ "[" ++ show n ++ "]"

data TypeShow = TypeEmpty | TypeWith Int

-- ---------------------------
data UFuncCall = UFuncCall LIde [LUExpr]

instance Show UFuncCall where
    show = dumpUFuncCall

dumpUFuncCall :: UFuncCall -> String
dumpUFuncCall (UFuncCall lide luexprs) =
    dumpIde (unLoc lide) ++ "(" ++ dumpLUExprs luexprs ++ ")"
