{
module Parser where
import Lexer
import Control.Monad.Writer(mempty, mappend)
}

%name parser Prog
%tokentype { Token }
%error { parseError }
%monad { Writer } { thenW } { returnW }

%nonassoc error
%left '+' '-'
%left '*' '/'
%nonassoc '(' ')'

%token
  int   { T_Int $$ }
  new_line { T_NewLine }
  '+'   { T_Plus }
  '-'   { T_Minus }
  '*'   { T_Mul }
  '/'   { T_Div }
  '('   { T_L }
  ')'   { T_R }


%%

Prog
  : Exp new_line    { $1 }

Exp ::              { Float }
  : '(' Exp ')'     { $2 }
  | Exp '*' Exp     { $1 * $3 }
  | Exp '/' Exp     { $1 / $3 }
  | Exp '+' Exp     { $1 + $3 }
  | Exp '-' Exp     { $1 - $3 }
  | int             { $1 }
  | error Errors    { -7.7 }

Errors
  : Any             { () }
  | Errors Any      { () }

Any
  : int             { () }
  | '+'             { () }
  | '-'             { () }
  | '*'             { () }
  | '/'             { () }
  | '('             { () }
  | ')'             { () }
  | error           { () }


{

newtype Writer a = Writer { runWriter :: (a, [String]) }

(Writer (x,v)) `thenW` f = let (Writer (y, v')) = f x in Writer (y, v `mappend` v')

returnW x = Writer (x, mempty)


parseError (t:tok) = error ( "grave error " ++ (show t) )


}
