--------------------------------------------------------------------------------
-- (c) Tsitsimpis Ilias, 2011
--
-- Untyped Abstract Syntax Tree for the Alan Language
-- This is our parser's output
--
--------------------------------------------------------------------------------

module UnTypedAst where


type Ide = String

data UDef
    = UDefFun Ide [UDef] UType [UDef] UStmt
    | UDefPar Ide Mode UType
    | UDefVar Ide UType
  deriving (Eq, Show)

data UStmt
    = UStmtNothing
    | UStmtAssign UVariable UExpr
    | UStmtCompound [UStmt]
    | UStmtFun UFuncCall
    | UStmtIf UCond UStmt (Maybe UStmt)
    | UStmtWhile UCond UStmt
    | UStmtReturn (Maybe UExpr)
  deriving (Eq, Show)

data UExpr
    = UExprInt Int
    | UExprChar Char
    | UExprString String
    | UExprVar UVariable
    | UExprFun UFuncCall
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

data UCond
    = UCondTrue
    | UCondFalse
    | UCondNot UCond
    | UCondOp UExpr Op UExpr
    | UCondLog UCond Op UCond
  deriving (Eq, Show)

data UVariable
    = UVar Ide
    | UVarArray Ide UExpr
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

data UFuncCall = UFuncCall Ide [UExpr]
  deriving (Eq, Show)
