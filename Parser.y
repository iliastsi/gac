{
module Parser where
import Lexer
}

%name parser Prog
%tokentype { Token }
%error { parseError }
%monad { P } { thenP } { returnP }

%left '+' '-'
%left '*' '/'
%nonassoc '(' ')'

%token
  int   { T_Int $$ }
  line  { T_NewLine }
  '+'   { T_Plus }
  '-'   { T_Minus }
  '*'   { T_Mul }
  '/'   { T_Div }
  '('   { T_L }
  ')'   { T_R }


%%

Prog
  : Exp line          { $1 }

Exp ::              { Float }
  : '(' Exp ')'     { $2 }
  | Exp '*' Exp     { $1 * $3 }
  | Exp '/' Exp     { $1 / $3 }
  | Exp '+' Exp     { $1 + $3 }
  | Exp '-' Exp     { $1 - $3 }
  | int             { $1 }


{

type P a = ParseResult a

data ParseResult a
  = Ok a
  | Failed String
  deriving (Show)

thenP :: P a -> (a -> P b) -> P b
m `thenP` k =
  case m of 
    Ok a -> (k a)
    Failed e -> Failed e

returnP :: a -> P a
returnP a = Ok a

failP :: String -> P a
failP err = Failed err

parseError (t:tok) = Failed ( "grave error " ++ (show t) )

}
