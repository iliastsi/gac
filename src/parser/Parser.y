-- -----------------------------------------------------------------------------
-- (c) Tsitsimpis Ilias, 2011
--
-- The GAC grammar
--
-- -----------------------------------------------------------------------------

{
{-# LANGUAGE BangPatterns #-} -- required for versions of Happy before 1.18.6
{-# OPTIONS_GHC -O0 -fno-ignore-interface-pragmas #-}
{-
Careful optimisation of the parser: we don't want to throw everything
at it, because that takes too long and doesn't buy much, but we do want
to inline certain key external functions, so we instruct GHC not to
throw away inlinings as it would normally do in -O0 mode.
-}

module Parser(parser) where

import Lexer
import ErrUtils
import SrcLoc
import UnTypedAst
}

%name parser program
%tokentype { (Located Token) }
%error { happyError }
%monad { P } { >>= } { return }
%lexer { lexer } { L _ ITeof }
%expect 1

%left '|'
%left '&'
%nonassoc '==' '!=' '>' '<' '<=' '>='
%left '+' '-'
%left '*' '/' '%'
%left '!' SIGN
%nonassoc '(' ')'

%token
    BYTE        { L _ ITbyte        }
    RETURN      { L _ ITreturn      }
    ELSE        { L _ ITelse        }
    WHILE       { L _ ITwhile       }
    FALSE       { L _ ITfalse       }
    TRUE        { L _ ITtrue        }
    IF          { L _ ITif          }
    INT         { L _ ITint         }
    PROC        { L _ ITproc        }
    REFERENCE   { L _ ITreference   }
    ID          { L _ (ITid _)      }
    DIGIT       { L _ (ITdigit _)   }
    CHAR        { L _ (ITchar _)    }
    STRING      { L _ (ITstring _)  }
    '='         { L _ ITassign      }
    '+'         { L _ ITplus        }
    '-'         { L _ ITminus       }
    '*'         { L _ ITtimes       }
    '/'         { L _ ITdiv         }
    '%'         { L _ ITmod         }
    '!'         { L _ ITnot         }
    '&'         { L _ ITand         }
    '|'         { L _ ITor          }
    '=='        { L _ ITequal       }
    '!='        { L _ ITnotequal    }
    '<'         { L _ ITlt          }
    '>'         { L _ ITgt          }
    '<='        { L _ ITle          }
    '>='        { L _ ITge          }
    '('         { L _ IToparen      }
    ')'         { L _ ITcparen      }
    '{'         { L _ ITocurly      }
    '}'         { L _ ITccurly      }
    '['         { L _ ITobrack      }
    ']'         { L _ ITcbrack      }
    ','         { L _ ITcomma       }
    ':'         { L _ ITcolon       }
    ';'         { L _ ITsemi        }



%% --Like yacc, we include %% here, for no real reason.

program :: { UDef }
    : funcdef                       { $1 }

funcdef :: { UDef }
    : ID fpar ':' rtype localdefs compoundstmt
                                    { let (ITid i) = (unLoc $1) in UDefFun i $2 $4 (reverse $5) (UStmtCompound $6) }
    | error                         {% srcParseFail "expected function decleration" }

fpar :: { [UDef] }
    : '(' ')'                       { [] }
    | '(' error                     {% srcParseFail "unclosed `)'" }
    | '(' fparlist ')'              { reverse $2 }
    | '(' fparlist error            {% srcParseFail "unclosed `)'" }
    | error                         {% srcParseFail "expected function definition parameters" }

fparlist :: { [UDef] }
    : fpardef                       { [$1] }
    | fparlist ',' fpardef          { $3 : $1 }

fpardef :: { UDef }
    : ID ':' type                   { let (ITid i) = (unLoc $1) in UDefPar i ModeByVal $3 }
    | ID ':' REFERENCE type         { let (ITid i) = (unLoc $1) in UDefPar i ModeByRef $4 }
    | ID error                      {% srcParseFail "missing `:'for type definition" }
    | error                         {% srcParseFail "expected parameter definition name" }

type :: { UType }
    : datatype                      { $1 }
    | datatype '[' ']'              { UTypeArray (0, $1) }
    | datatype '[' error            {% srcParseFail "expected `]'" }
    | error                         {% srcParseFail "unknown datatype" }

datatype :: { UType }
    : INT                           { UTypeInt }
    | BYTE                          { UTypeChar }

rtype :: { UType}
    : datatype                      { $1 }
    | PROC                          { UTypeProc }
    | error                         {% srcParseFail "unknown datatype" }

localdefs :: { [UDef] }
    : {- nothing -}                 { [] }
    | localdefs localdef            { $2 : $1 }

localdef :: { UDef }
    : funcdef                       { $1 }
    | vardef                        { $1 }

vardef :: { UDef }
    : ID ':' datatype ';'           { let (ITid i) = (unLoc $1) in UDefVar i $3 }
    | ID ':' datatype '[' DIGIT ']' ';'
                                    { let (ITid x) = (unLoc $1); (ITdigit y) = (unLoc $5) in
                                        UDefVar x (UTypeArray (y, $3)) }
    | ID ':' datatype '[' DIGIT ']' error
                                    {% srcParseFail "missing `;'" }
    | ID ':' datatype '[' DIGIT error
                                    {% srcParseFail "unclosed `]'" }
    | ID ':' datatype '[' error     {% srcParseFail "array size must be an integer" }
    | ID ':' datatype error         {% srcParseFail "missing `;'" }
    | ID error                      {% srcParseFail "missing `:' for type definition" }

stmt :: { UStmt }
    : ';'                           { UStmtNothing }
    | lvalue '=' expr ';'           { UStmtAssign $1 $3 }
    | lvalue '=' expr error         {% srcParseFail "missing `;'" }
    | lvalue '=' error              {% srcParseFail "not a valid expression" }
    | ID error                      {% srcParseFail "expected an lvalue or `(' for function" }
    | compoundstmt                  { UStmtCompound $1 }
    | funcall ';'                   { UStmtFun $1 }
    | funcall error                 {% srcParseFail "missing `;'" }
    | IF '(' cond ')' stmt          { UStmtIf $3 $5 Nothing}
    | IF '(' cond ')' stmt ELSE stmt
                                    { UStmtIf $3 $5 (Just $7) }
    | IF '(' cond error             {% srcParseFail "unclosed `)'" }
    | IF '(' error                  {% srcParseFail "not a valid condition" }
    | IF error                      {% srcParseFail "expected `('" }
    | WHILE '(' cond ')' stmt       { UStmtWhile $3 $5 }
    | WHILE '(' cond error          {% srcParseFail "unclosed `)'" }
    | WHILE '(' error               {% srcParseFail "not a valid condition" }
    | WHILE error                   {% srcParseFail "expected `('" }
    | RETURN ';'                    { UStmtReturn Nothing }
    | RETURN error                  {% srcParseFail "missing `;'" }
    | RETURN expr ';'               { UStmtReturn (Just $2) }
    | RETURN expr error             {% srcParseFail "missing `;'" }
    | error                         {% srcParseFail "not a valid statement" }

compoundstmt :: { [UStmt] }
    : '{' stmts '}'                 { reverse $2 }

stmts :: { [UStmt] }
    : {- nothing -}                 { [] }
    | stmts stmt                    { $2 : $1 }

funcall :: { UFuncCall }
    : ID '(' ')'                    { let (ITid i) = (unLoc $1) in UFuncCall i [] }
    | ID '(' error                  {% srcParseFail "unclosed `)'" }
    | ID '(' exprlist ')'           { let (ITid i) = (unLoc $1) in UFuncCall i (reverse $3) }
    | ID '(' exprlist error         {% srcParseFail "unclosed `)'" }

exprlist :: { [UExpr] }
    : expr                          { [$1] }
    | exprlist ',' expr             { $3 : $1 }
    | exprlist ',' error            {% srcParseFail "not a valid expression" }

expr :: { UExpr }
    : DIGIT                         { let (ITdigit i) = (unLoc $1) in UExprInt i }
    | CHAR                          { let (ITchar i) = (unLoc $1) in UExprChar i }
    | STRING                        { let (ITstring i) = (unLoc $1) in UExprString i }
    | lvalue                        { UExprVar $1 }
    | '(' expr ')'                  { $2 }
    | '(' expr error                {% srcParseFail "unclosed `)'" }
    | '(' error                     {% srcParseFail "not a valid expression" }
    | funcall                       { UExprFun $1 }
    | '+' expr %prec SIGN           { $2 }
    | '+' error %prec SIGN          {% srcParseFail "not a valid expression" }
    | '-' expr %prec SIGN           { UExprOp (UExprInt 0) OpMinus $2 }
    | '-' error %prec SIGN          {% srcParseFail "not a valid expression" }
    | expr '+' expr                 { UExprOp $1 OpPlus  $3 }
    | expr '+' error                {% srcParseFail "not a valid expression" }
    | expr '-' expr                 { UExprOp $1 OpMinus $3 }
    | expr '-' error                {% srcParseFail "not a valid expression" }
    | expr '*' expr                 { UExprOp $1 OpTimes $3 }
    | expr '*' error                {% srcParseFail "not a valid expression" }
    | expr '/' expr                 { UExprOp $1 OpDiv   $3 }
    | expr '/' error                {% srcParseFail "not a valid expression" }
    | expr '%' expr                 { UExprOp $1 OpMod   $3 }
    | expr '%' error                {% srcParseFail "not a valid expression" }

lvalue :: { UVariable }
    : ID                            { let (ITid i) = (unLoc $1) in UVar i }
    | ID '[' expr ']'               { let (ITid i) = (unLoc $1) in UVarArray i $3 }
    | ID '[' expr error             {% srcParseFail "unclosed `]'" }
    | ID '[' error                  {% srcParseFail "not a valid expression" }

cond :: { UCond }
    : TRUE                          { UCondTrue }
    | FALSE                         { UCondFalse }
    | '(' cond ')'                  { $2 }
    | '(' cond error                {% srcParseFail "unclosed `)'" }
    | '!' cond                      { UCondNot $2 }
    | '!' error                     {% srcParseFail "not a valid condition" }
    | expr '==' expr                { UCondOp  $1 OpEqual    $3 }
    | expr '==' error               {% srcParseFail "not a valid expression" }
    | expr '!=' expr                { UCondOp  $1 OpNotEqual $3 }
    | expr '!=' error               {% srcParseFail "not a valid expression" }
    | expr '<'  expr                { UCondOp  $1 OpLT       $3 }
    | expr '<'  error               {% srcParseFail "not a valid expression" }
    | expr '>'  expr                { UCondOp  $1 OpGT       $3 }
    | expr '>'  error               {% srcParseFail "not a valid expression" }
    | expr '<=' expr                { UCondOp  $1 OpLE       $3 }
    | expr '<=' error               {% srcParseFail "not a valid expression" }
    | expr '>=' expr                { UCondOp  $1 OpGE       $3 }
    | expr '>=' error               {% srcParseFail "not a valid expression" }
    | cond '&'  cond                { UCondLog $1 OpAnd      $3 }
    | cond '&'  error               {% srcParseFail "not a valid condition" }
    | cond '|'  cond                { UCondLog $1 OpOr       $3 }
    | cond '|'  error               {% srcParseFail "not a valid condition" }


{

happyError :: (Located Token) -> P a
happyError _  = srcParseFail ""

}
