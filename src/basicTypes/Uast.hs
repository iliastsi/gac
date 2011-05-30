--------------------------------------------------------------------------------
-- (c) Tsitsimpis Ilias, 2011
--
-- Untyped Abstract Syntax Tree for the Alan Language
-- This is our parser's output
--
--------------------------------------------------------------------------------

module Uast where


type Ide = String

data Def
    = DefFun Ide [Def] UType [Def] Stmt
    | DefPar Ide Mode UType
    | DefVar Ide UType
  deriving (Eq, Show)

data Stmt
    = StmtNothing
    | StmtAssign UValue UExpr
    | StmtCompound [Stmt]
    | StmtFun UFunCall
    | StmtIf Cond Stmt (Maybe Stmt)
    | StmtWhile Cond Stmt
    | StmtReturn (Maybe UExpr)
  deriving (Eq, Show)

data UExpr
    = UExprInt Int
    | UExprChar Char
    | UExprString String
    | UExprVal UValue
    | UExprPar UExpr
    | UExprFun UFunCall
    | UExprSign Op UExpr
    | UExprOp UExpr Op UExpr
  deriving (Eq, Show)

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

data Cond
    = CondTrue
    | CondFalse
    | CondPar Cond
    | CondNot Cond
    | CondOp UExpr Op UExpr
    | CondLog Cond Op Cond
  deriving (Eq, Show)

data UValue
    = UVal Ide
    | UValArray Ide UExpr
  deriving (Eq, Show)

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

data UFunCall = UFunCall Ide [UExpr]
    deriving (Eq, Show)
