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
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module Lexer (
    ParseResult(..), PState, P(..), mkPState, Token(..),
    failMsgP, failLocMsgP, failSpanMsgP, failSpanTokP, srcParseFail,
    lexer, lexDummy, getPState,
    getInput, setInput, AlexInput(..),
    getSrcLoc, setSrcLoc,
    getPMessages,
    addPWarning, addPError
  ) where

import SrcLoc
import ErrUtils
import DynFlags
import Outputable (panic)

import Data.Char
import Data.Word (Word8)
import qualified Data.ByteString.Lazy.Char8 as BSC
import qualified Codec.Binary.UTF8.String as Codec

}


$digit = 0-9
$alpha = [a-z A-Z]
$id = [$alpha \_ $digit]
$char = $printable # [\'\"\\]
$hexit = [$digit a-f A-F]

@special = \\n | \\t | \\r | \\0 | \\\\ | \\\' | \\\" | \\x $hexit $hexit


tokens :-

$white+         ;

<0> {
  byte          { token ITbyte }
  return        { token ITreturn }
  else          { token ITelse }
  while         { token ITwhile }
  false         { token ITfalse }
  true          { token ITtrue }
  if            { token ITif }
  int           { token ITint }
  proc          { token ITproc }
  reference     { token ITreference }

  $alpha $id*   { lex_id_tok }
  $digit+       { lex_int_tok }
  \' ($char|@special) \'
                { lex_char_tok }
  \" ($char|\'|@special)* \"
                { lex_string_tok }

  "="           { token ITassign }
  "+"           { token ITplus }
  "-"           { token ITminus }
  "*"           { token ITtimes }
  "/"           { token ITdiv }
  "%"           { token ITmod }
  "!"           { token ITnot }
  "&"           { token ITand }
  "|"           { token ITor }
  "=="          { token ITequal }
  "!="          { token ITnotequal }
  "<"           { token ITlt }
  ">"           { token ITgt }
  "<="          { token ITle }
  ">="          { token ITge }
  "("           { token IToparen }
  ")"           { token ITcparen }
  "["           { token ITobrack }
  "]"           { token ITcbrack }
  "{"           { token ITocurly }
  "}"           { token ITccurly }
  ","           { token ITcomma }
  ":"           { token ITcolon }
  ";"           { token ITsemi }

  "*)"          { errorMsg ("You are trying to close the comments without having opened them first") }
}

"--" .*         ;
"(*"            { embedComment }
<comments> {
  "*)"          { unembedComment }
  .             ;
}

.               { unknownChar }


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
    | ITdigit Integer
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
type Action = SrcSpan -> BSC.ByteString -> Int -> P (Located Token)

token :: Token -> Action
token t span _buf _len = return (L span t)

skip :: Action
skip _span _buf _len = lexToken

andBegin :: Action -> Int -> Action
andBegin act code span buf len = do setLexState code; act span buf len

begin :: Int -> Action
begin code = skip `andBegin` code

lex_id_tok :: Action
lex_id_tok span buf len = do
    let ide = take len $ BSC.unpack buf
    ide `seq` return $ L span (ITid ide)

lex_int_tok :: Action
lex_int_tok span buf len = do
    case BSC.readInteger buf of
        Just (num, _) -> num `seq` return $ L span (ITdigit num)
        Nothing       -> panic "Lexer.lex_int_tok did not return an integer"

lex_string_tok :: Action
lex_string_tok span buf len = do
    let tok_string = escape $ take (len-2) $ tail (BSC.unpack buf)
    tok_string `seq` return $ L span (ITstring tok_string)

lex_char_tok :: Action
lex_char_tok span buf len = do
    let c = head $ escape $ take (len-2) $ tail (BSC.unpack buf)
    c `seq` return $ L span (ITchar c)

-- strip out special characters from strings
escape :: String -> String
escape ('\\' : 'n'  : s)         = '\n' : escape s
escape ('\\' : 't'  : s)         = '\t' : escape s
escape ('\\' : 'r'  : s)         = '\r' : escape s
escape ('\\' : '0'  : s)         = '\0' : escape s
escape ('\\' : '\\' : s)         = '\\' : escape s
escape ('\\' : '\'' : s)         = '\'' : escape s
escape ('\\' : '\"' : s)         = '\"' : escape s
escape ('\\' : 'x'  : y : z : s) = chr (read ("0x" ++ (y:z:[]))) : escape s
escape ('\\' :  x   : s)         =  x   : escape s
escape (  x  :  s) = x : escape s
escape [] = []

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
unknownChar span buf len = do
    let unk_c = [BSC.head buf]
    unk_c `seq` errorMsg ("Cannot parse char `" ++ unk_c ++ "'") span buf len

-- -------------------------------------------------------------------
-- Warnings and Errors

warnMsg :: String -> Action
warnMsg msg span buf len = do
    let tok = take len $ BSC.unpack buf
    tok `seq` addPWarning span tok msg
    lexToken

warnThen :: String -> Action -> Action
warnThen msg action span buf len = do
    let tok = take len $ BSC.unpack buf
    tok `seq` addPWarning span tok msg
    action span buf len

errorMsg :: String -> Action
errorMsg msg span buf len = do
    let tok = take len $ BSC.unpack buf
    tok `seq` addPError span tok msg
    lexToken

errorThen :: String -> Action -> Action
errorThen msg action span buf len = do
    let tok = take len $ BSC.unpack buf
    tok `seq` addPError span tok msg
    action span buf len

-- -------------------------------------------------------------------
-- The Parse Monad

data ParseResult a
    = POk PState a
    | PFailed Messages

data PState = PState {
    buffer	        :: BSC.ByteString,
    dflags          :: DynFlags,
    messages        :: Messages,
    last_loc        :: SrcSpan, -- pos of previous token
    last_tok        :: !String, -- string of the previous token
    loc             :: SrcLoc,  -- current loc (end of token + 1)
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
    PFailed (addError (mkErrMsg span (ParseError tok) msg) ms)

failMsgP :: String -> P a
failMsgP msg = P $ \s@(PState{messages=ms, last_loc=span, last_tok=tok}) ->
    PFailed (addError (mkErrMsg span (ParseError tok) msg) ms)

failLocMsgP :: SrcLoc -> SrcLoc -> String -> P a
failLocMsgP loc1 loc2 msg = P $ \s@(PState{messages=ms, last_tok=tok}) ->
    PFailed (addError (mkErrMsg (mkSrcSpan loc1 loc2) (ParseError tok) msg) ms)

failSpanMsgP :: SrcSpan -> String -> P a
failSpanMsgP span msg = P $ \s@(PState{messages=ms, last_tok=tok}) ->
    PFailed (addError (mkErrMsg span (ParseError tok) msg) ms)

failSpanTokP :: SrcSpan -> String -> String -> P a
failSpanTokP span tok msg = P $ \s@(PState{messages=ms}) ->
    PFailed (addError (mkErrMsg span (ParseError tok) msg) ms)

getPState :: P PState
getPState = P $ \s -> POk s s

getInput :: P AlexInput
getInput = P $ \s@(PState{loc=loc, buffer=buffer}) ->
                    POk s (AI loc buffer [])

setInput :: AlexInput -> P ()
setInput (AI loc buf _) = P $ \s ->
                    POk s{loc=loc, buffer=buf} ()

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

data AlexInput =
    AI SrcLoc           -- current position,
       BSC.ByteString   -- current input string,
       [Word8]          -- rest of the bytes for the current char

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar _ =
    panic $ "Lexer.alexInputPrevChar supposed to be undefined."
            ++ "\n\tWe don't use patterns with a left-context."

alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte (AI loc buf (r:rs)) =
        Just (r, AI loc buf rs)
alexGetByte (AI loc buf [])
    | BSC.null buf  = Nothing
    | otherwise =
        let c      = BSC.head buf
            (r:rs) = Codec.encode [c]
            buf'   = BSC.tail buf
            loc'   = advanceSrcLoc loc c
        in c `seq` loc' `seq` Just (r, AI loc' buf' rs)

-- for compat with Alex 2.x:
alexGetChar :: AlexInput -> Maybe (Char, AlexInput)
alexGetChar i =
    case alexGetByte i of
        Nothing     -> Nothing
        Just (b,i') ->
            case i' of
                (AI _ _ []) ->
                    Just (chr (fromIntegral b), i')
                otherwise     ->
                    panic $ "Alex version 2.x doesn't support UTF-8."
                            ++ "\n\tPlease compile with Alex version 3.x"

-- create a parse state
--
mkPState :: DynFlags -> BSC.ByteString -> SrcLoc -> PState
mkPState flags buf loc =
  PState {
    buffer          = buf,
    dflags          = flags,
    messages        = emptyMessages,
    last_loc        = mkSrcSpan loc loc,
    last_tok        = "",
    loc             = loc,
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

getPMessages :: PState -> Messages
getPMessages PState{messages=ms} = ms

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
               then errorThen ("You forgot to close the comments")
                        (\_ _ _ -> return (L span ITeof)) span buf 0
               else return (L span ITeof)
        AlexError (AI loc2 buf2 _) ->
            reportLexError loc1 loc2 buf2 "Unknown lexical error"
        AlexSkip inp2 _ -> do
            setInput inp2
            lexToken
        AlexToken inp2@(AI end _ []) len t -> do
            setInput inp2
            let span = mkSrcSpan loc1 end
            span `seq` setLastToken span (take len $ BSC.unpack buf)
            t (mkSrcSpan loc1 end) buf len
        AlexToken _ _ _ ->
            panic $ "AlexToken in Lexer.lexToken didn't eat all the"
                    ++ "bytes for the current char"

reportLexError :: SrcLoc -> SrcLoc -> BSC.ByteString -> String -> P a
reportLexError loc1 loc2 buf str
    | BSC.null buf  = failLocMsgP loc1 loc2 (str ++ " at end of input")
    | otherwise =
        let c = BSC.head buf
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
