--------------------------------------------------------------------------------
-- (c) Tsitsimpis Ilias, 2011
--
-- GAC's lexer.
--
-- This is a combination of an Alex-generated lexer from a regex
-- definition, with some hand-coded bits.
--
-- Completely accurate information about token-spans within the source
-- file is maintained.  Every token has a start and end SrcLoc attached to it.
--
--------------------------------------------------------------------------------

{
-- XXX The above flags turn off warnings in the generated code:
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
-- But alex still generates some code that causes the "lazy unlifted bindings"
-- warning, and old compilers don't know about it so we can't easily turn
-- it off, so for now we use the sledge hammer:
{-# OPTIONS_GHC -w #-}

{-# OPTIONS_GHC -funbox-strict-fields #-}

module Lexer (
    ParseResult(..), PState(..), P(..), mkPState, Token(..),
    failMsgP, failLocMsgP, failSpanMsgP, srcParseFail,
    lexer, lexDummy, getPState,
    getInput, setInput, AlexInput(..),
    getSrcLoc, setSrcLoc,
    getMessages,
    addPWarning, addPError
  ) where

import SrcLoc
import ErrUtils

import Data.Char

}


$digit = 0-9
$alpha = [a-z A-Z]
$id = [$alpha \_ $digit]
$char = $printable # [\'\"\\]
$hexit = [$digit a-f A-F]

@special = \\n | \\t | \\r | \\0 | \\\\ | \\\' | \\\" | \\x $hexit $hexit


tokens :-

$white+             ;

<0> {
  byte              { token ITbyte }
  return            { token ITreturn }
  else              { token ITelse }
  while             { token ITwhile }
  false             { token ITfalse }
  true              { token ITtrue }
  if                { token ITif }
  int               { token ITint }
  proc              { token ITproc }
  reference         { token ITreference }

  $alpha $id*       { lex_id_tok }
  $digit+           { lex_int_tok }
  \' ($char|@special) \'
                    { lex_char_tok }
  \" ($char|@special)* \"
                    { lex_string_tok }

  "="               { token ITassign }
  "+"               { token ITplus }
  "-"               { token ITminus }
  "*"               { token ITtimes }
  "/"               { token ITdiv }
  "%"               { token ITmod }
  "!"               { token ITnot }
  "&"               { token ITand }
  "|"               { token ITor }
  "=="              { token ITequal }
  "!="              { token ITnotequal }
  "<"               { token ITlt }
  ">"               { token ITgt }
  "<="              { token ITle }
  ">="              { token ITge }
  "("               { token IToparen }
  ")"               { token ITcparen }
  "["               { token ITobrack }
  "]"               { token ITcbrack }
  "{"               { token ITocurly }
  "}"               { token ITccurly }
  ","               { token ITcomma }
  ":"               { token ITcolon }
  ";"               { token ITsemi }

  "*)"              { errorMsg ("unmatched `*)'") }
}

"--" .*             ;
"(*"                { embedComment }
<comments> {
  "*)"              { unembedComment }
  .                 ;
}

.                   { unknownChar }


{

data Token
    = ITbyte
    | ITreturn
    | ITelse
    | ITwhile
    | ITfalse
    | ITtrue
    | ITif
    | ITint
    | ITproc
    | ITreference

    | ITid String
    | ITdigit Int
    | ITchar Char
    | ITstring String

    | ITassign      -- =
    | ITplus        -- +
    | ITminus       -- -
    | ITtimes       -- *
    | ITdiv         -- /
    | ITmod         -- %
    | ITnot         -- !
    | ITand         -- &
    | ITor          -- |
    | ITequal       -- ==
    | ITnotequal    -- !=
    | ITlt          -- <
    | ITgt          -- >
    | ITle          -- <=
    | ITge          -- >=
    | IToparen      -- (
    | ITcparen      -- )
    | ITocurly      -- {
    | ITccurly      -- }
    | ITobrack      -- [
    | ITcbrack      -- ]
    | ITcomma       -- ,
    | ITcolon       -- :
    | ITsemi        -- ;

    | ITeof             -- end of file token
    deriving Eq

-- -------------------------------------------------------------------
-- Lexer actions

-- Position -> Buffer -> Length -> P Token
type Action = SrcSpan -> String -> Int -> P (Located Token)

token :: Token -> Action
token t span _buf _len = return (L span t)

skip :: Action
skip _span _buf _len = lexToken

andBegin :: Action -> Int -> Action
andBegin act code span buf len = do setLexState code; act span buf len

begin :: Int -> Action
begin code = skip `andBegin` code

lex_id_tok :: Action
lex_id_tok span buf len = return (L span (ITid (take len buf)))

lex_int_tok :: Action
lex_int_tok span buf len = do
    let num_str = take len buf
        num     = read num_str
    if (num <= 32768) && (num >= -32769)
       then return (L span (ITdigit num))
       else warnThen ("number `" ++ show num ++ "' is bigger than 16 bits")
                (\_ _ _ -> return (L span (ITdigit num))) span buf len

lex_string_tok :: Action    -- strip out \" from beginng and ending
lex_string_tok span buf len = return (L span (ITstring (take (len-2) (tail buf))))

lex_char_tok :: Action
lex_char_tok span buf len = return (L span (ITchar c))
    where c = case take (len-2) (tail buf) of -- stip \' from beginning and ending
                    "\\n"    -> '\n'
                    "\\t"    -> '\t'
                    "\\r"    -> '\r'
                    "\\0"    -> '\0'
                    "\\\\"    -> '\\'
                    "\\\'"    -> '\''
                    "\\\""    -> '\"'
                    ('\\':'x':x)    -> chr $ read ("0x" ++ x)
                    ('\\':x:[])     -> x
                    (x:[])          -> x
                    _               -> error "in lex_char_tok"

embedComment :: Action
embedComment span buf len = do
    incCommState
    begin comments span buf len

unembedComment :: Action
unembedComment span buf len = do
    decCommState
    status <- getCommState
    if status == 0
        then begin 0 span buf len
        else lexToken

unknownChar :: Action
unknownChar span buf len =
    errorMsg ("Cannot parse char `" ++ take 1 buf ++ "'") span buf len

-- -------------------------------------------------------------------
-- Warnings and Errors

warnMsg :: String -> Action
warnMsg msg span buf len = do
    addPWarning span (take len buf) msg
    lexToken

warnThen :: String -> Action -> Action
warnThen msg action span buf len = do
    addPWarning span (take len buf) msg
    action span buf len

errorMsg :: String -> Action
errorMsg msg span buf len = do
    addPError span (take len buf) msg
    lexToken

errorThen :: String -> Action -> Action
errorThen msg action span buf len = do
    addPError span (take len buf) msg
    action span buf len

-- -------------------------------------------------------------------
-- The Parse Monad

data ParseResult a
    = POk PState a
    | PFailed Messages

data PState = PState { 
    buffer	        :: String,
    messages        :: Messages,
    last_loc        :: SrcSpan, -- pos of previous token
    last_tok        :: !String, -- string of the previous token
    loc             :: SrcLoc,  -- current loc (end of token + 1)
    prev            :: !Char,   -- previous char
	lex_state       :: !Int,
    comment_state   :: !Int
  }

newtype P a = P { unP :: PState -> ParseResult a }

instance Monad P where
    return = returnP
    (>>=) = thenP
    fail = failP

returnP :: a -> P a
returnP a = a `seq` (P $ \s -> POk s a)

thenP :: P a -> (a -> P b) -> P b
(P m) `thenP` k = P $ \ s ->
    case m s of
        POk s1 a     -> (unP (k a)) s1
        PFailed msgs -> PFailed msgs

failP :: String -> P a
failP msg = P $ \s@(PState{messages=ms, last_loc=span, last_tok=tok}) ->
    PFailed (addError (mkErrMsg span (ParseError tok)msg) ms)

failMsgP :: String -> P a
failMsgP msg = P $ \s@(PState{messages=ms, last_loc=span, last_tok=tok}) ->
    PFailed (addError (mkErrMsg span (ParseError tok) msg) ms)

failLocMsgP :: SrcLoc -> SrcLoc -> String -> P a
failLocMsgP loc1 loc2 msg = P $ \s@(PState{messages=ms, last_tok=tok}) ->
    PFailed (addError (mkErrMsg (mkSrcSpan loc1 loc2) (ParseError tok) msg) ms)

failSpanMsgP :: SrcSpan -> String -> P a
failSpanMsgP span msg = P $ \s@(PState{messages=ms, last_tok=tok}) ->
    PFailed (addError (mkErrMsg span (ParseError tok) msg) ms)

getPState :: P PState
getPState = P $ \s -> POk s s

getInput :: P AlexInput
getInput = P $ \s@(PState{loc=loc, buffer=buffer, prev=prev}) ->
                    POk s (AI loc buffer prev)

setInput :: AlexInput -> P ()
setInput (AI loc buf prev) = P $ \s ->
                    POk s{loc=loc, buffer=buf, prev=prev} ()

getLexState :: P Int
getLexState = P $ \s@(PState{lex_state=lex_state}) ->
                    POk s lex_state

setLexState :: Int ->P ()
setLexState new_state = P $ \s ->
                    POk s{lex_state=new_state} ()

setSrcLoc :: SrcLoc -> P ()
setSrcLoc new_loc = P $ \s -> POk s{loc=new_loc} ()

getSrcLoc :: P SrcLoc
getSrcLoc = P $ \s@(PState{loc=loc}) -> POk s loc

setLastToken :: SrcSpan -> String -> P ()
setLastToken loc str = P $ \s -> POk s {
    last_loc=loc,
    last_tok=str
    } ()

incCommState :: P ()
incCommState = P $ \s@(PState{comment_state=prev}) ->
                    POk s{comment_state=prev+1} ()

decCommState :: P ()
decCommState = P $ \s@(PState{comment_state=prev}) ->
                    POk s{comment_state=prev-1} ()

getCommState :: P Int
getCommState = P $ \s@(PState{comment_state=comment_state}) ->
                    POk s comment_state

data AlexInput = AI SrcLoc String Char

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (AI _ _ c) = c

alexGetChar :: AlexInput -> Maybe (Char, AlexInput)
alexGetChar (AI _   []     _) = Nothing
alexGetChar (AI loc (x:xs) _) = Just (x, AI (advanceSrcLoc loc x) xs x)


-- create a parse state
--
mkPState :: String -> SrcLoc -> PState
mkPState buf loc =
  PState {
    buffer          = buf,
    messages        = emptyMessages,
    last_loc        = mkSrcSpan loc loc,
    last_tok        = "",
    loc             = loc,
    prev            = '\n',
    lex_state       = 0,
    comment_state   = 0
  }

addPWarning :: SrcSpan -> String -> String -> P ()
addPWarning loc tok msg
    = P $ \s@(PState{messages=msgs}) ->
        POk s{ messages=(addWarning (mkWarnMsg loc (ParseError tok) msg) msgs) } ()

addPError :: SrcSpan -> String -> String -> P ()
addPError loc tok msg
    = P $ \s@(PState{messages=msgs}) ->
        POk s{ messages=(addError (mkErrMsg loc (ParseError tok) msg) msgs) } ()

getMessages :: PState -> Messages
getMessages PState{messages=ms} = ms

-- -------------------------------------------------------------------
-- Construct a parse error

-- Report a parse failure, giving the span of the previous token as
-- the location of the error. This is the entry point for errors
-- detected during parsing.
srcParseFail :: String -> P a
srcParseFail msg = P $ \PState{messages=ms, last_loc=span, last_tok=tok} ->
    PFailed (addError (mkErrMsg span (ParseError tok) msg) ms)

-- A lexical error is reported at a particular position
-- in the source file, not over a token range.
lexError :: String -> P a
lexError str = do
    loc <- getSrcLoc
    (AI end buf _) <- getInput
    reportLexError loc end buf str

-- -------------------------------------------------------------------
-- This is the top-level functions: lexer is called from the parser
-- each time a new token is to be read from the input.

lexToken :: P (Located Token)
lexToken = do
    inp@(AI loc1 buf _) <- getInput
    sc <- getLexState
    case alexScan inp sc of
        AlexEOF -> do
            let span = mkSrcSpan loc1 loc1
            setLastToken span ""
            if sc > 0
               then errorThen ("unterminated `(*'")
                        (\_ _ _ -> return (L span ITeof)) span buf 0
               else return (L span ITeof)
        AlexError (AI loc2 buf2 _) ->
            reportLexError loc1 loc2 buf2 "Unknown lexical error"
        AlexSkip inp2 _ -> do
            setInput inp2
            lexToken
        AlexToken inp2@(AI end _ _) len t -> do
            setInput inp2
            let span = mkSrcSpan loc1 end
            span `seq` setLastToken span (take len buf)
            t (mkSrcSpan loc1 end) buf len

reportLexError :: SrcLoc -> SrcLoc -> String -> String -> P a
reportLexError loc1 loc2 buf str
    | null buf  = failLocMsgP loc1 loc2 (str ++ " at end of input")
    | otherwise =
        let c = head buf
        in
        failLocMsgP loc1 loc2 (str ++ " at character " ++ show c)

lexer :: (Located Token -> P a) -> P a
lexer cont = do
  tok@(L _span _tok__) <- lexToken
  cont tok

-- used for debugging as it not contains continuation function
--
lexDummy :: P [(Located Token)]
lexDummy = do
    tok@(L _span t) <- lexToken
    if t==ITeof
        then do let toks = []
                P $ \s -> POk s (tok : toks)
        else do toks <- lexDummy
                P $ \s -> POk s (tok : toks)

}
