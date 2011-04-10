{
module Parser where
import Lexer
}

%name parser Prog
%tokentype { Token }
%error { parseError }
%monad { P }
%lexer { lexer } { T_EOF }

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
  line  { T_NewLine }


%%

Prog
  : Exp line        { $1 }

Exp :
    '(' Exp ')'     { $2 }
  | '(' Exp error Anys   {% P $ \inp sc -> (0, ["Unclosed Brasset"]) }
  | Exp '*' Exp     { $1 * $3 }
  | Exp '*' error   {% P $ \inp sc -> (0, ["Unclosed Mult"]) }
  | Exp '/' Exp     { $1 / $3 }
  | Exp '+' Exp     { $1 + $3 }
  | Exp '-' Exp     { $1 - $3 }
  | int             { $1 }

Anys
  : {- nothing -}   { () }
  | Anys Any        { () }

Any
  : int   { () }
  | '+'   { () }
  | '-'   { () }
  | '*'   { () }
  | '/'   { () }
  | '('   { () }
  | ')'   { () }


{

--parseError :: P a
parseError = error ("Error in token: ")-- ++ (show tk))

}
