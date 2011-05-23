module AstTypes where


type Ide = String

data AST_def
    = DefFun Ide [AST_def] AST_type [AST_def] [AST_stmt]
    | DefPar Ide AST_mode AST_type
    | DefVar Ide AST_type
  deriving (Eq, Show)

data AST_stmt
    = StmtNothing
    | StmtAssign AST_value AST_expr
    | StmtCompound [AST_stmt]
    | StmtFun FunCall
    | StmtIf AST_cond AST_stmt (Maybe AST_stmt)
    | StmtWhile AST_cond AST_stmt
    | StmtReturn (Maybe AST_expr)
  deriving (Eq, Show)

data AST_expr
    = ExprInt Int
    | ExprChar Char
    | ExprString String
    | ExprVal AST_value
    | ExprPar AST_expr
    | ExprFun FunCall
    | ExprSign AST_op AST_expr
    | ExprOp AST_expr AST_op AST_expr
  deriving (Eq, Show)

data AST_op
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

data AST_cond
    = CondTrue
    | CondFalse
    | CondPar AST_cond
    | CondNot AST_cond
    | CondOp AST_expr AST_op AST_expr
    | CondLog AST_cond AST_op AST_cond
  deriving (Eq, Show)

data AST_value
    = Val Ide
    | ValArray Ide AST_expr
  deriving (Eq, Show)

data AST_type
    = TypeInt
    | TypeChar
    | TypeProc
    | TypeArray (Int, AST_type)
  deriving (Eq, Show)

data AST_mode
    = ModeByVal
    | ModeByRef
  deriving (Eq, Show)

data FunCall = FunCall Ide [AST_expr]
    deriving (Eq, Show)

type RetType = Maybe AST_type

sizeof :: AST_type -> Int
sizeof TypeInt  = 2
sizeof TypeChar = 1
sizeof TypeProc = 1
sizeof (TypeArray (n,t)) = n * sizeof t
