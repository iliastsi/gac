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
}

%name parser Program
%tokentype { Token }
%error { happyError }
%monad { P }
%lexer { lexer } { T_EOF }

%left '|'
%left '&'
%nonassoc '==' '!=' '>' '<' '<=' '>='
%left '+' '-'
%left '*' '/' '%'
%left '!' SIGN
%nonassoc '(' ')'

%token
    kwByte      { T_kwByte      }
    return      { T_Return      }
    else        { T_Else        }
    while       { T_While       }
    false       { T_False       }
    true        { T_True        }
    if          { T_If          }
    kwInt       { T_kwInt       }
    proc        { T_Proc        }
    reference   { T_Reference   }
    id          { T_Id      $$  }
    int         { T_Int     $$  }
    char        { T_Char    $$  }
    string      { T_String  $$  }
    '='         { T_Assign      }
    '+'         { T_Plus        }
    '-'         { T_Minus       }
    '*'         { T_Times       }
    '/'         { T_Div         }
    '%'         { T_Mod         }
    '!'         { T_Not         }
    '&'         { T_And         }
    '|'         { T_Or          }
    '=='        { T_Eq          }
    '!='        { T_Ne          }
    '<'         { T_Lt          }
    '>'         { T_Gt          }
    '<='        { T_Le          }
    '>='        { T_Ge          }
    '('         { T_Op          }
    ')'         { T_Cp          }
    '['         { T_Os          }
    ']'         { T_Cs          }
    '{'         { T_Oc          }
    '}'         { T_Cc          }
    ','         { T_Comma       }
    ':'         { T_Colon       }
    ';'         { T_SemiColon   }



%%

Program
    : FuncDef                       { () }

FuncDef
    : id '(' FparList ')' ':' RType LocalDefs '{' CompoundStmt '}'
                                    { () }

FparList
    : {- nothing -}                 { () }
    | FparDef                       { () }
    | FparList ',' FparDef          { () }

FparDef
    : id ':' Type                   { () }
    | id ':' reference Type         { () }

DataType
    : kwInt                         { () }
    | kwByte                        { () }

Type
    : DataType                      { () }
    | DataType '[' ']'              { () }

RType
    : DataType                      { () }
    | proc                          { () }

LocalDefs
    : {- nothing -}                 { () }
    | LocalDefs LocalDef            { () }

LocalDef
    : FuncDef                       { () }
    | VarDef                        { () }

VarDef
    : id ':' DataType ';'           { () }
    | id ':' DataType '[' int ']' ';'
                                    { () }

Stmt
    : ';'                           { () }
    | LValue '=' Expr ';'           { () }
    | '{' CompoundStmt '}'          { () }
    | FuncCall ';'                  { () }
    | if '(' Cond ')' Stmt          { () }
    | if '(' Cond ')' Stmt else Stmt
                                    { () }
    | while '(' Cond ')' Stmt       { () }
    | return ';'                    { () }
    | return Expr ';'               { () }

CompoundStmt
    : {- nothing -}                 { () }
    | CompoundStmt Stmt             { () }

FuncCall
    : id '(' ')'                    { () }
    | id '(' ExprList ')'           { () }

ExprList
    : Expr                          { () }
    | ExprList ',' Expr             { () }

Expr
    : int                           { () }
    | char                          { () }
    | LValue                        { () }
    | '(' Expr ')'                  { () }
    | FuncCall                      { () }
    | '+' Expr %prec SIGN           { () }
    | '-' Expr %prec SIGN           { () }
    | Expr '+' Expr                 { () }
    | Expr '-' Expr                 { () }
    | Expr '*' Expr                 { () }
    | Expr '/' Expr                 { () }
    | Expr '%' Expr                 { () }

LValue
    : id                            { () }
    | id '[' Expr ']'               { () }
    | string                        { () }

Cond
    : true                          { () }
    | false                         { () }
    | '(' Cond ')'                  { () }
    | '!' Cond                      { () }
    | Expr '==' Expr                { () }
    | Expr '!=' Expr                { () }
    | Expr '<'  Expr                { () }
    | Expr '>'  Expr                { () }
    | Expr '<=' Expr                { () }
    | Expr '>=' Expr                { () }
    | Cond '&'  Cond                { () }
    | Cond '|'  Cond                { () }


{

happyError :: Token -> P a
happyError T_EOF =
    P $ \(pos,_,str) sc -> error $
        parseError "Happy internal error at end of file" pos
happyError _  =
    P $ \(pos,_,str) sc -> error $
        parseError ("Happy internal error befor " ++ (take 10 str)) pos

}
