{
module Parser where
import Lexer
import Control.Monad.Writer(mempty, mappend)
}

%name parser Prog
%tokentype { Token }
%error { parseError }
%monad { Writer } { thenW } { returnW }

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
  | '(' Exp error Anys   {% Writer (0, ["Unclosed Brasset"]) }
  | Exp '*' Exp     { $1 * $3 }
  | Exp '*' error   {% Writer (0, ["Unclosed Mult"]) }
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

newtype Writer a = Writer { runWriter :: (a, [String]) }

(Writer (x,v)) `thenW` f = let (Writer (y, v')) = f x in Writer (y, v `mappend` v')

returnW x = Writer (x, mempty)


parseError _ = error "my parse Error"


}
