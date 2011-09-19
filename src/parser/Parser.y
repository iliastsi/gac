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

{-
-- -------------------------------------------------------------------
-- Adding location info

This is done in a stylised way using the three macros below, L0, L1
and LL.  Each of these macros can be thought of as having type

   L0, L1, LL :: a -> Located a

They each add a SrcSpan to their argument.

   L0   adds 'noSrcSpan', used for empty productions
     -- This doesn't seem to work anymore -=chak

   L1   for a production with a single token.  Grabs the SrcSpan
        from that token.

   LL   for a production with >1 token.  Makes up a SrcSpan from
        the first and last tokens.

These suffice for the majority of cases.  However, we must be
especially careful with empty productions: LL won't work if the first
or last token on the lhs can represent an empty span.  In these cases,
we have to calculate the span using more of the tokens, eg.

        | 'newtype' tycl_hdr '=' newconstr deriving
                { L (comb3 $1 $4 $5)
                    (mkTyData NewType (unLoc $2) [$4] (unLoc $5)) }

We provide comb3 and comb4 functions which are useful in such cases.

Be careful: there's no checking that you actually got this right, the
only symptom will be that the SrcSpans of your syntax will be
incorrect.

/*
 * We must expand these macros *before* running Happy, which is why this file is
 * Parser.y.pp rather than just Parser.y - we run the C pre-processor first.
 */
#define L0   L noSrcSpan
#define L1   sL (getLoc $1)
#define LL   sL (comb2 $1 $>)

-- -------------------------------------------------------------------
-}

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

program :: { Located UDef }
    : funcdef                       { $1 }

funcdef :: { Located UDef }
    : ID fpar ':' rtype localdefs compoundstmt
                                    { sL (comb2 $1 $>) $ let (L span (ITid i)) = $1 in UDefFun (sL span i) $2 $4 (reverse $5) $6 }
    | error                         {% srcParseFail "expected function decleration" }

fpar :: { [Located UDef] }
    : '(' ')'                       { [] }
    | '(' error                     {% srcParseFail "unclosed `)'" }
    | '(' fparlist ')'              { reverse $2 }
    | '(' fparlist error            {% srcParseFail "unclosed `)'" }
    | error                         {% srcParseFail "expected function definition parameters" }

fparlist :: { [Located UDef] }
    : fpardef                       { [$1] }
    | fparlist ',' fpardef          { $3 : $1 }

fpardef :: { Located UDef }
    : ID ':' type                   { sL (comb2 $1 $>) $ let (L span (ITid i)) = $1 in UDefPar (sL span i) ModeByVal $3 }
    | ID ':' REFERENCE type         { sL (comb2 $1 $>) $ let (L span (ITid i)) = $1 in UDefPar (sL span i) ModeByRef $4 }
    | ID error                      {% srcParseFail "missing `:'for type definition" }
    | error                         {% srcParseFail "expected parameter definition name" }

type :: { Located UType }
    : datatype                      { $1 }
    | datatype '[' ']'              { sL (comb2 $1 $>) $ UTypeArray (0, unLoc $1) }
    | datatype '[' error            {% srcParseFail "expected `]'" }
    | error                         {% srcParseFail "unknown datatype" }

datatype :: { Located UType }
    : INT                           { sL (getLoc $1) $ UTypeInt }
    | BYTE                          { sL (getLoc $1) $ UTypeChar }

rtype :: { Located UType}
    : datatype                      { $1 }
    | PROC                          { sL (getLoc $1) $ UTypeProc }
    | error                         {% srcParseFail "unknown datatype" }

localdefs :: { [Located UDef] }
    : {- nothing -}                 { [] }
    | localdefs localdef            { $2 : $1 }

localdef :: { Located UDef }
    : funcdef                       { $1 }
    | vardef                        { $1 }

vardef :: { Located UDef }
    : ID ':' datatype ';'           { sL (comb2 $1 $>) $ let (L span (ITid i)) = $1 in UDefVar (sL span i) $3 }
    | ID ':' datatype '[' DIGIT ']' ';'
                                    { sL (comb2 $1 $>) $ let (L span (ITid i)) = $1; (ITdigit d) = unLoc $5 in UDefVar (sL span i) (sL (comb2 $3 $6) (UTypeArray (d, unLoc $3))) }
    | ID ':' datatype '[' DIGIT ']' error
                                    {% srcParseFail "missing `;'" }
    | ID ':' datatype '[' DIGIT error
                                    {% srcParseFail "unclosed `]'" }
    | ID ':' datatype '[' error     {% srcParseFail "array size must be an integer" }
    | ID ':' datatype error         {% srcParseFail "missing `;'" }
    | ID error                      {% srcParseFail "missing `:' for type definition" }

stmt :: { Located UStmt }
    : ';'                           { sL (getLoc $1) $ UStmtNothing }
    | lvalue '=' expr ';'           { sL (comb2 $1 $>) $ UStmtAssign $1 $3 }
    | lvalue '=' expr error         {% srcParseFail "missing `;'" }
    | lvalue '=' error              {% srcParseFail "not a valid expression" }
    | ID error                      {% srcParseFail "expected an lvalue or `(' for function" }
    | compoundstmt                  { $1 }
    | funcall ';'                   { sL (comb2 $1 $>) $ UStmtFun (unLoc $1) }
    | funcall error                 {% srcParseFail "missing `;'" }
    | IF '(' cond ')' stmt          { sL (comb2 $1 $>) $ UStmtIf $3 $5 Nothing}
    | IF '(' cond ')' stmt ELSE stmt
                                    { sL (comb2 $1 $>) $ UStmtIf $3 $5 (Just $7) }
    | IF '(' cond error             {% srcParseFail "unclosed `)'" }
    | IF '(' error                  {% srcParseFail "not a valid condition" }
    | IF error                      {% srcParseFail "expected `('" }
    | WHILE '(' cond ')' stmt       { sL (comb2 $1 $>) $ UStmtWhile $3 $5 }
    | WHILE '(' cond error          {% srcParseFail "unclosed `)'" }
    | WHILE '(' error               {% srcParseFail "not a valid condition" }
    | WHILE error                   {% srcParseFail "expected `('" }
    | RETURN ';'                    { sL (comb2 $1 $>) $ UStmtReturn Nothing }
    | RETURN error                  {% srcParseFail "missing `;'" }
    | RETURN expr ';'               { sL (comb2 $1 $>) $ UStmtReturn (Just $2) }
    | RETURN expr error             {% srcParseFail "missing `;'" }
    | error                         {% srcParseFail "not a valid statement" }

compoundstmt :: { Located UStmt }
    : '{' stmts '}'                 { sL (comb2 $1 $>) $ UStmtCompound (reverse $2) }

stmts :: { [Located UStmt] }
    : {- nothing -}                 { [] }
    | stmts stmt                    { $2 : $1 }

funcall :: { Located UFuncCall }
    : ID '(' ')'                    { sL (comb2 $1 $>) $ let (L span (ITid i)) = $1 in UFuncCall (sL span i) [] }
    | ID '(' error                  {% srcParseFail "unclosed `)'" }
    | ID '(' exprlist ')'           { sL (comb2 $1 $>) $ let (L span (ITid i)) = $1 in UFuncCall (sL span i) (reverse $3) }
    | ID '(' exprlist error         {% srcParseFail "unclosed `)'" }

exprlist :: { [Located UExpr] }
    : expr                          { [$1] }
    | exprlist ',' expr             { $3 : $1 }
    | exprlist ',' error            {% srcParseFail "not a valid expression" }

expr :: { Located UExpr }
    : DIGIT                         { sL (getLoc $1) $ let (ITdigit i) = (unLoc $1) in UExprInt i }
    | CHAR                          { sL (getLoc $1) $ let (ITchar i) = (unLoc $1) in UExprChar i }
    | STRING                        { sL (getLoc $1) $ let (ITstring i) = (unLoc $1) in UExprString i }
    | lvalue                        { sL (getLoc $1) $ let v = unLoc $1 in UExprVar v }
    | '(' expr ')'                  { sL (comb2 $1 $>) $ let e = unLoc $2 in e}
    | '(' expr error                {% srcParseFail "unclosed `)'" }
    | '(' error                     {% srcParseFail "not a valid expression" }
    | funcall                       { sL (getLoc $1) $ let f = unLoc $1 in UExprFun f }
    | '+' expr %prec SIGN           { sL (comb2 $1 $>) $ let e = unLoc $2 in e}
    | '+' error %prec SIGN          {% srcParseFail "not a valid expression" }
    | '-' expr %prec SIGN           { sL (comb2 $1 $>) $ UExprMinus $2 }
    | '-' error %prec SIGN          {% srcParseFail "not a valid expression" }
    | expr '+' expr                 { sL (comb2 $1 $>) $ UExprOp $1 (sL (getLoc $2) OpPlus) $3 }
    | expr '+' error                {% srcParseFail "not a valid expression" }
    | expr '-' expr                 { sL (comb2 $1 $>) $ UExprOp $1 (sL (getLoc $2) OpMinus) $3 }
    | expr '-' error                {% srcParseFail "not a valid expression" }
    | expr '*' expr                 { sL (comb2 $1 $>) $ UExprOp $1 (sL (getLoc $2) OpTimes) $3 }
    | expr '*' error                {% srcParseFail "not a valid expression" }
    | expr '/' expr                 { sL (comb2 $1 $>) $ UExprOp $1 (sL (getLoc $2) OpDiv) $3 }
    | expr '/' error                {% srcParseFail "not a valid expression" }
    | expr '%' expr                 { sL (comb2 $1 $>) $ UExprOp $1 (sL (getLoc $2) OpMod) $3 }
    | expr '%' error                {% srcParseFail "not a valid expression" }

lvalue :: { Located UVariable }
    : ID                            { sL (getLoc $1) $ let (ITid i) = unLoc $1 in UVar i }
    | ID '[' expr ']'               { sL (comb2 $1 $>) $ let (L span (ITid i)) = $1 in UVarArray (sL span i) $3 }
    | ID '[' expr error             {% srcParseFail "unclosed `]'" }
    | ID '[' error                  {% srcParseFail "not a valid expression" }

cond :: { Located UCond }
    : TRUE                          { sL (getLoc $1) $ UCondTrue }
    | FALSE                         { sL (getLoc $1) $ UCondFalse }
    | '(' cond ')'                  { sL (comb2 $1 $>) $ let c = unLoc $2 in c }
    | '(' cond error                {% srcParseFail "unclosed `)'" }
    | '!' cond                      { sL (comb2 $1 $>) $ UCondNot $2 }
    | '!' error                     {% srcParseFail "not a valid condition" }
    | expr '==' expr                { sL (comb2 $1 $>) $ UCondOp $1 (sL (getLoc $2) OpEqual) $3 }
    | expr '==' error               {% srcParseFail "not a valid expression" }
    | expr '!=' expr                { sL (comb2 $1 $>) $ UCondOp $1 (sL (getLoc $2) OpNotEqual) $3 }
    | expr '!=' error               {% srcParseFail "not a valid expression" }
    | expr '<'  expr                { sL (comb2 $1 $>) $ UCondOp $1 (sL (getLoc $2) OpLT) $3 }
    | expr '<'  error               {% srcParseFail "not a valid expression" }
    | expr '>'  expr                { sL (comb2 $1 $>) $ UCondOp $1 (sL (getLoc $2) OpGT) $3 }
    | expr '>'  error               {% srcParseFail "not a valid expression" }
    | expr '<=' expr                { sL (comb2 $1 $>) $ UCondOp $1 (sL (getLoc $2) OpLE) $3 }
    | expr '<=' error               {% srcParseFail "not a valid expression" }
    | expr '>=' expr                { sL (comb2 $1 $>) $ UCondOp $1 (sL (getLoc $2) OpGE) $3 }
    | expr '>=' error               {% srcParseFail "not a valid expression" }
    | cond '&'  cond                { sL (comb2 $1 $>) $ UCondLog $1 (sL (getLoc $2) OpAnd) $3 }
    | cond '&'  error               {% srcParseFail "not a valid condition" }
    | cond '|'  cond                { sL (comb2 $1 $>) $ UCondLog $1 (sL (getLoc $2) OpOr) $3 }
    | cond '|'  error               {% srcParseFail "not a valid condition" }


{

happyError :: (Located Token) -> P a
happyError _  = srcParseFail ""

-- Utilities for combining source spans
comb2 :: Located a -> Located b -> SrcSpan
comb2 a b = a `seq` b `seq` combineLocs a b

comb3 :: Located a -> Located b -> Located c -> SrcSpan
comb3 a b c = a `seq` b `seq` c `seq`
    combineSrcSpans (getLoc a) (combineSrcSpans (getLoc b) (getLoc c))

comb4 :: Located a -> Located b -> Located c -> Located d -> SrcSpan
comb4 a b c d = a `seq` b `seq` c `seq` d `seq`
    (combineSrcSpans (getLoc a) $ combineSrcSpans (getLoc b) $
                combineSrcSpans (getLoc c) (getLoc d))

-- strict constructor version:
{-# INLINE sL #-}
sL :: SrcSpan -> a -> Located a
sL span a = span `seq` a `seq` L span a

}
