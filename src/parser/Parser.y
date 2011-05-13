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



%%

program
    : funcDef                       { () }

funcDef
    : ID '(' fparList ')' ':' rType localDefs '{' compoundStmt '}'
                                    { () }

fparList
    : {- nothing -}                 { () }
    | fparDef                       { () }
    | fparList ',' fparDef          { () }

fparDef
    : ID ':' type                   { () }
    | ID ':' REFERENCE type         { () }

dataType
    : INT                           { () }
    | BYTE                          { () }

type
    : dataType                      { () }
    | dataType '[' ']'              { () }

rType
    : dataType                      { () }
    | PROC                          { () }

localDefs
    : {- nothing -}                 { () }
    | localDefs localDef            { () }

localDef
    : funcDef                       { () }
    | varDef                        { () }

varDef
    : ID ':' dataType ';'           { () }
    | ID ':' dataType '[' DIGIT ']' ';'
                                    { () }

stmt
    : ';'                           { () }
    | lValue '=' expr ';'           { () }
    | '{' compoundStmt '}'          { () }
    | funcCall ';'                  { () }
    | IF '(' cond ')' stmt          { () }
    | IF '(' cond ')' stmt ELSE stmt
                                    { () }
    | WHILE '(' cond ')' stmt       { () }
    | RETURN ';'                    { () }
    | RETURN expr ';'               { () }

compoundStmt
    : {- nothing -}                 { () }
    | compoundStmt stmt             { () }

funcCall
    : ID '(' ')'                    { () }
    | ID '(' exprList ')'           { () }

exprList
    : expr                          { () }
    | exprList ',' expr             { () }

expr
    : DIGIT                         { () }
    | CHAR                          { () }
    | lValue                        { () }
    | '(' expr ')'                  { () }
    | funcCall                      { () }
    | '+' expr %prec SIGN           { () }
    | '-' expr %prec SIGN           { () }
    | expr '+' expr                 { () }
    | expr '-' expr                 { () }
    | expr '*' expr                 { () }
    | expr '/' expr                 { () }
    | expr '%' expr                 { () }

lValue
    : ID                            { () }
    | ID '[' expr ']'               { () }
    | STRING                        { () }

cond
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


{

happyError :: (Located Token) -> P a
happyError (L _ ITeof) = lexError "Happy internal error at end of file"
happyError (L _ _)     = lexError "Happy my error"

}
