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

program :: { () }
    : funcdef                       { () }

funcdef :: { () }
    : ID fpar ':' rtype localdefs compoundstmt
                                    { () }
    | ID fpar err_funcdef           { () }
    | err_funcdef                   { () }

fpar :: { () }
    : '(' ')'                       { () }
    | '(' err_fpar                  { () }
    | '(' fparlist ')'              { () }
    | '(' fparlist err_fpar         { () }
    | err_fpar                      { () }

fparlist :: { () }
    : fpardef                       { () }
    | fparlist ',' fpardef          { () }

fpardef :: { () }
    : ID ':' type                   { () }
    | ID ':' REFERENCE type         { () }

datatype :: { () }
    : INT                           { () }
    | BYTE                          { () }

type :: { () }
    : datatype                      { () }
    | datatype '[' ']'              { () }

rtype :: { () }
    : datatype                      { () }
    | PROC                          { () }

localdefs :: { () }
    : {- nothing -}                 { () }
    | localdefs localdef            { () }

localdef :: { () }
    : funcdef                       { () }
    | vardef                        { () }

vardef :: { () }
    : ID ':' datatype ';'           { () }
    | ID ':' datatype '[' DIGIT ']' ';'
                                    { () }

stmt :: { () }
    : ';'                           { () }
    | lvalue '=' expr ';'           { () }
    | compoundstmt                  { () }
    | funccall ';'                  { () }
    | IF '(' cond ')' stmt          { () }
    | IF '(' cond ')' stmt ELSE stmt
                                    { () }
    | WHILE '(' cond ')' stmt       { () }
    | RETURN ';'                    { () }
    | RETURN expr ';'               { () }

compoundstmt :: { () }
    : '{' stmts '}'                 { () }

stmts :: { () }
    : {- nothing -}                 { () }
    | stmts stmt                    { () }

funccall :: { () }
    : ID '(' ')'                    { () }
    | ID '(' exprlist ')'           { () }

exprlist :: { () }
    : expr                          { () }
    | exprlist ',' expr             { () }

expr :: { () }
    : DIGIT                         { () }
    | CHAR                          { () }
    | lvalue                        { () }
    | '(' expr ')'                  { () }
    | funccall                      { () }
    | '+' expr %prec SIGN           { () }
    | '-' expr %prec SIGN           { () }
    | expr '+' expr                 { () }
    | expr '-' expr                 { () }
    | expr '*' expr                 { () }
    | expr '/' expr                 { () }
    | expr '%' expr                 { () }

lvalue :: { () }
    : ID                            { () }
    | ID '[' expr ']'               { () }
    | STRING                        { () }

cond :: { () }
    : TRUE                          { () }
    | FALSE                         { () }
    | '(' cond ')'                  { () }
    | '!' cond                      { () }
    | expr '==' expr                { () }
    | expr '!=' expr                { () }
    | expr '<'  expr                { () }
    | expr '>'  expr                { () }
    | expr '<=' expr                { () }
    | expr '>=' expr                { () }
    | cond '&'  cond                { () }
    | cond '|'  cond                { () }


-- -------------------------------------------------------------------
-- Error Rule

err_funcdef :: { () }
    : errors localdefs compoundstmt
                                    {%^ \(L pos tok) -> do
                                        { addError pos "Parse error in funcdef";
                                          return () } }

errors :: { () }
    : error                         {%% \_ -> return () }
    | errors error                  {%% \_ -> return () }


{

happyError :: (Located Token) -> P a
happyError (L _ ITeof) = failMsgP "Happy internal error at end of file"
happyError (L loc tok)     = failLocMsgP loc ("Happy internal error in " ++ (show tok))

}
