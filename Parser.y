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

type E a = Either String a

thenE :: E a -> (a -> E b) -> E b
m `thenE` k =
  case m of
    Right a -> k a
    Left e  -> Left e

returnE :: a -> E a
returnE a = Right a

failE :: String -> E a
failE err = Left err


parseError tok             = failE "error"

}
