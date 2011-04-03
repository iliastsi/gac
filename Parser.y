{
module Parser where
import Lexer
}

%name parser
%tokentype { Token }
%error { parseError }

%right '+'
%right '-'
%right '*'
%right '/'

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
    '(' Exp ')'       { $2 }
  | Exp '*' Exp       { $1 * $3 }
  | Exp '/' Exp       { $1 / $3 }
  | Exp_              { $1 }

Exp_ :
    '(' Exp_ ')'      { $2 }
  | Exp_ '+' Exp_     { $1 + $3 }
  | Exp_ '-' Exp_     { $1 - $3 }
  | int               { $1 }


{

parseError :: [Token] -> a
parseError _ = error "Parse Error"

}
