{
module Parser where
import Lexer
}

%name parser
%tokentype { Token }
%error { parseError }

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
    int '+' int       { $1 + $3 }


{

parseError :: [Token] -> a
parseError _ = error "Parse Error"

}
