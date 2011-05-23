-- -----------------------------------------------------------------------------
-- (c) Tsitsimpis Ilias, 2011
--
-- The GAC grammar.
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
import AstTypes
}

%name parser program
%tokentype { (Located Token) }
%error { happyError }
%monad { P } { >>= } { return }
%lexer { lexer } { L _ ITeof }

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

program :: { AST_def }
    : funcdef                       { $1 }

funcdef :: { AST_def }
    : ID fpar ':' rtype localdefs compoundstmt
                                    { let (ITid i) = (unLoc $1) in DefFun i $2 $4 $5 $6 }

fpar :: { [AST_def] }
    : '(' ')'                       { [] }
    | '(' fparlist ')'              { $2 }

fparlist :: { [AST_def] }
    : fpardef                       { [$1] }
    | fparlist ',' fpardef          { $3 : $1 }

fpardef :: { AST_def }
    : ID ':' type                   { let (ITid i) = (unLoc $1) in DefPar i ModeByVal $3 }
    | ID ':' REFERENCE type         { let (ITid i) = (unLoc $1) in DefPar i ModeByRef $4 }

type :: { AST_type }
    : datatype                      { $1 }
    | datatype '[' ']'              { TypeArray (0, $1) }

datatype :: { AST_type }
    : INT                           { TypeInt }
    | BYTE                          { TypeChar }

rtype :: { AST_type}
    : datatype                      { $1 }
    | PROC                          { TypeProc }

localdefs :: { [AST_def] }
    : {- nothing -}                 { [] }
    | localdefs localdef            { $2 : $1 }

localdef :: { AST_def }
    : funcdef                       { $1 }
    | vardef                        { $1 }

vardef :: { AST_def }
    : ID ':' datatype ';'           { let (ITid i) = (unLoc $1) in DefVar i $3 }
    | ID ':' datatype '[' DIGIT ']' ';'
                                    { let (ITid x) = (unLoc $1); (ITdigit y) = (unLoc $5) in
                                        DefVar x (TypeArray (y, $3)) }

stmt :: { AST_stmt }
    : ';'                           { StmtNothing }
    | lvalue '=' expr ';'           { StmtAssign $1 $3 }
    | compoundstmt                  { StmtCompound $1 }
    | funcall ';'                   { StmtFun $1 }
    | IF '(' cond ')' stmt          { StmtIf $3 $5 Nothing}
    | IF '(' cond ')' stmt ELSE stmt
                                    { StmtIf $3 $5 (Just $7) }
    | WHILE '(' cond ')' stmt       { StmtWhile $3 $5 }
    | RETURN ';'                    { StmtReturn Nothing }
    | RETURN expr ';'               { StmtReturn (Just $2) }

compoundstmt :: { [AST_stmt] }
    : '{' stmts '}'                 { $2 }

stmts :: { [AST_stmt] }
    : {- nothing -}                 { [] }
    | stmts stmt                    { $2 : $1 }

funcall :: { FunCall }
    : ID '(' ')'                    { let (ITid i) = (unLoc $1) in FunCall i [] }
    | ID '(' exprlist ')'           { let (ITid i) = (unLoc $1) in FunCall i $3 }

exprlist :: { [AST_expr] }
    : expr                          { [$1] }
    | exprlist ',' expr             { $3 : $1 }

expr :: { AST_expr }
    : DIGIT                         { let (ITdigit i) = (unLoc $1) in ExprInt i }
    | CHAR                          { let (ITchar i) = (unLoc $1) in ExprChar i }
    | STRING                        { let (ITstring i) = (unLoc $1) in ExprString i }
    | lvalue                        { ExprVal $1 }
    | '(' expr ')'                  { ExprPar $2 }
    | funcall                       { ExprFun $1 }
    | '+' expr %prec SIGN           { ExprSign OpPlus $2 }
    | '-' expr %prec SIGN           { ExprSign OpMinus $2 }
    | expr '+' expr                 { ExprOp $1 OpPlus $3 }
    | expr '-' expr                 { ExprOp $1 OpMinus $3 }
    | expr '*' expr                 { ExprOp $1 OpTimes $3 }
    | expr '/' expr                 { ExprOp $1 OpDiv $3 }
    | expr '%' expr                 { ExprOp $1 OpMod $3 }

lvalue :: { AST_value }
    : ID                            { let (ITid i) = (unLoc $1) in Val i }
    | ID '[' expr ']'               { let (ITid i) = (unLoc $1) in ValArray i $3 }

cond :: { AST_cond }
    : TRUE                          { CondTrue }
    | FALSE                         { CondFalse }
    | '(' cond ')'                  { CondPar $2 }
    | '!' cond                      { CondNot $2 }
    | expr '==' expr                { CondOp $1 OpEqual $3 }
    | expr '!=' expr                { CondOp $1 OpNotEqual $3 }
    | expr '<'  expr                { CondOp $1 OpLT $3 }
    | expr '>'  expr                { CondOp $1 OpGT $3 }
    | expr '<=' expr                { CondOp $1 OpLE $3 }
    | expr '>=' expr                { CondOp $1 OpGE $3 }
    | cond '&'  cond                { CondLog $1 OpAnd $3 }
    | cond '|'  cond                { CondLog $1 OpOr $3 }


{

happyError :: (Located Token) -> P a
happyError (L _ ITeof) = failMsgP "Happy internal error at end of file"
happyError (L loc tok)     = failLocMsgP loc ("Happy internal error in " ++ (show tok))

}
