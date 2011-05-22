module AstTypes where


type Ide = String

data AST_type
    = AST_integer
    | AST_boolean
    | AST_char
    | AST_array    (Int, AST_type)

data AST_mode
    = AST_byval
    | AST_byref

type RetType = Maybe AST_type
type FuncID = Int
type TempId = Int

sizeof AST_integer  = 2
sizeof AST_boolean  = 1
sizeof AST_char     = 1
sizeof (AST_array (n,t)) = n * sizeof t
