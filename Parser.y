{
module Parser where
import Lexer
}

%partial parser Prog
%tokentype { Token }
%error { happyError }
%monad { P }
%lexer { lexer } { T_EOF }

%left '+' '-'
%left '*' '/'
%nonassoc '(' ')'

%token
  int   { T_Int $$ }
  '+'   { T_Plus }
  '-'   { T_Minus }
  '*'   { T_Times }
  '('   { T_Op }
  ')'   { T_Cp }
  line  { T_NewLine }


%%

Prog
  : Exp             { $1 }
  | Exp error       { 0 }

Exp :
    '(' Exp ')'     { $2 }
  | '(' Exp error Anys   
                    {% P $ \(p,_,_) sc -> (0, ([parseError "Unclosed Bracket" p],[])) }
  | Exp '*' Exp     { $1 * $3 }
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
  | '('   { () }
  | ')'   { () }


{

happyError :: Token -> P a
happyError _ = P $ \inp sc -> error ("We have to put something in here")

}
