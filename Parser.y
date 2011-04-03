{
module Parser where
import Lexer
}

%name parser
%tokentype { Token }
%error { parseError }
%monad { E } { thenE } { returnE }

%left '+' '-'
%left '*' '/'
%nonassoc '(' ')'

%token
  int   { T_Int $$ }
  '+'   { T_Plus }
  '-'   { T_Minus }
  '*'   { T_Mul }
  '/'   { T_Div }
  '('   { T_L }
  ')'   { T_R }


%%

Exp :
    '(' Exp ')'     {% returnE $2 }
  | Exp '*' Exp     { $1 * $3 }
  | Exp '/' Exp     { $1 / $3 }
  | Exp '+' Exp     { $1 + $3 }
  | Exp '-' Exp     { $1 - $3 }
  | int             { $1 }


{

data E a = Ok a | Failed String deriving (Show)

thenE :: E a -> (a -> E b) -> E b
m `thenE` k =
  case m of
    Ok a     -> k a
    Failed e -> Failed e

returnE :: a -> E a
returnE a = Ok a

failE :: String -> E a
failE err = Failed err

catchE :: E a -> (String -> E a) -> E a
catchE m k =
  case m of
    Ok a     -> Ok a
    Failed e -> k e

parseError tok             = failE "error"

}
