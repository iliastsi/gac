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
  '*'   { T_Times }
  '/'   { T_Div }
  '('   { T_Op }
  ')'   { T_Cp }
  line  { T_NewLine }


%%

Prog
  : Exp             { $1 }

Exp :
    '(' Exp ')'     { $2 }
  | '(' Exp error Anys   
                    {% P $ \(p,_,_) sc -> (0, [parseWarning "Unclosed Bracket" p]) }
  | Exp '*' Exp     { $1 * $3 }
  | Exp '*' error   {% P $ \(p,_,_) sc -> (0, [parseWarning "Unclosed Mult" p]) }
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

parseError :: Token -> P a
parseError _ = P $ \inp sc -> error ("We have to put something in here")

parseWarning :: String ->AlexPosn -> String
parseWarning msg pos = (showPosn pos ++ ":  " ++ msg)

}
