--------------------------------------------------------------------------------
-- (c) Tsitsimpis Ilias, 2011
--
-- Untyped Abstract Syntax Tree for the Alan Language
-- This is our parser's output
--
--------------------------------------------------------------------------------

module UnTypedAst where

import SrcLoc


type LIde = Located Ide

type Ide = String

type LUDef = Located UDef

data UDef
    = UDefFun LIde [LUDef] LUType [LUDef] LUStmt
    | UDefPar LIde Mode LUType
    | UDefVar LIde LUType
  deriving (Eq, Show)

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

type LUExpr = Located UExpr

data UExpr
    = UExprInt Int
    | UExprChar Char
    | UExprString String
    | UExprVar UVariable
    | UExprFun UFuncCall
    | UExprMinus LUExpr
    | UExprOp LUExpr LOp LUExpr
  deriving (Eq, Show)

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
  deriving (Eq, Show)

type LUCond = Located UCond

data UCond
    = UCondTrue
    | UCondFalse
    | UCondNot LUCond
    | UCondOp LUExpr LOp LUExpr
    | UCondLog LUCond LOp LUCond
  deriving (Eq, Show)

type LUVariable = Located UVariable

data UVariable
    = UVar Ide
    | UVarArray LIde LUExpr
  deriving (Eq, Show)

type LUType = Located UType

data UType
    = UTypeInt
    | UTypeChar
    | UTypeProc
    | UTypeArray (Int, UType)
  deriving (Eq, Show)

data Mode
    = ModeByVal
    | ModeByRef
  deriving (Eq, Show)

data UFuncCall = UFuncCall LIde [LUExpr]
  deriving (Eq, Show)
