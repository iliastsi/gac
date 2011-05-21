--------------------------------------------------------------------------------
-- (c) Tsitsimpis Ilias, 2011
--
-- GAC's lexer.
--
-- This is a combination of an Alex-generated lexer from a regex
-- definition, with some hand-coded bits.
--
-- Completely accurate information about token-spans within the source
-- file is maintained.  Every token has a SrcLoc attached to it.
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
    ParseResult(..), PState(..), P(..), mkPState,
    failMsgP, failLocMsgP, Token(..),
    lexer, lexDummy, getPState,
    getInput, setInput, AlexInput(..),
    getSrcLoc, setSrcLoc,
    addError, addWarning, getMessages
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

  "*)"              { errorMsg "Unmatched comment close symbol" }
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
    deriving (Eq, Show)

-- -------------------------------------------------------------------
-- Lexer actions

-- Position -> Buffer -> Length -> P Token
type Action = SrcLoc -> String -> Int -> P (Located Token)

token :: Token -> Action
token t pos _buf _len = return (L pos t)

skip :: Action
skip _pos _buf _len = lexToken

andBegin :: Action -> Int -> Action
andBegin act code pos buf len = do setLexState code; act pos buf len

begin :: Int -> Action
begin code = skip `andBegin` code

lex_id_tok :: Action
lex_id_tok pos buf len = return (L pos (ITid (take len buf)))

lex_int_tok :: Action
lex_int_tok pos buf len = do
    let num_str = take len buf
        num     = read num_str
    if num <= 32768
       then return (L pos (ITdigit num))
       else warnThen ("Number " ++ num_str ++ " is bigger than 16 bits")
                (\_ _ _ -> return (L pos (ITdigit num))) pos buf len

lex_string_tok :: Action    -- strip out \" from beginng and ending
lex_string_tok pos buf len = return (L pos (ITstring (take (len-2) (tail buf))))

lex_char_tok :: Action
lex_char_tok pos buf len = return (L pos (ITchar c))
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
embedComment pos buf len = do
    incCommState
    begin comments pos buf len

unembedComment :: Action
unembedComment pos buf len = do
    decCommState
    status <- getCommState
    if status == 0
        then begin 0 pos buf len
        else lexToken

unknownChar :: Action
unknownChar pos buf len = do
    let (c:_) = buf
        msg = "Unknown char " ++
            (if isPrint c
                then show c
                else "with ascii code " ++ (show $ ord c))
    errorMsg msg pos buf len

-- -------------------------------------------------------------------
-- Warnings and Errors

warnMsg :: String -> Action
warnMsg msg pos _buf _len = do
    addWarning pos msg
    lexToken

warnThen :: String -> Action -> Action
warnThen msg action pos buf len = do
    addWarning pos msg
    action pos buf len

errorMsg :: String -> Action
errorMsg msg pos _buf _len = do
    addError pos msg
    lexToken

errorThen :: String -> Action -> Action
errorThen msg action pos buf len = do
    addError pos msg
    action pos buf len

-- -------------------------------------------------------------------
-- The Parse Monad

data ParseResult a
    = POk PState a
    | PFailed 
      SrcLoc        -- The end of the text span related to the error
      Message       -- The error message
  deriving Show

data PState = PState { 
    buffer	        :: String,
    messages        :: Messages,
    loc             :: SrcLoc,  -- current loc (end of token + 1)
    prev            :: !Char,   -- previous char
	lex_state       :: !Int,
    comment_state   :: !Int
  } deriving Show

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
        POk s1 a         -> (unP (k a)) s1
        PFailed span err -> PFailed span err

failP :: String -> P a
failP msg = P $ \s@(PState{loc=loc}) -> PFailed loc msg

failMsgP :: String -> P a
failMsgP msg = P $ \s@(PState{loc=loc}) -> PFailed loc msg

failLocMsgP :: SrcLoc -> String -> P a
failLocMsgP loc msg = P $ \_ -> PFailed loc msg

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
alexGetChar (AI loc ('\n':xs) _) = Just ('\n', AI (incSrcLine loc) xs '\n')
alexGetChar (AI loc ('\t':xs) _) = Just ('\t', AI (incSrcTab loc) xs '\t')
alexGetChar (AI loc ('\r':xs) _) = Just ('\r', AI (srcCarRet loc) xs '\r')
alexGetChar (AI loc (x:xs) _)    = Just (x, AI (incSrcColumn loc) xs x)
alexGetChar (AI _ [] _) = Nothing


-- create a parse state
--
mkPState :: String -> SrcLoc -> PState
mkPState buf loc =
  PState {
    buffer          = buf,
    messages        = emptyMessages,
    loc             = loc,
    prev            = '\n',
    lex_state       = 0,
    comment_state   = 0
  }

addWarning :: SrcLoc -> String -> P ()
addWarning loc msg
    = P $ \s@(PState{messages=(ws,es)}) ->
        let warning' = mkLocWarnMsg loc msg
            ws'     = ws `snocBag` warning'
        in POk s{messages=(ws',es)} ()

addError :: SrcLoc -> String -> P ()
addError loc msg
    = P $ \s@(PState{messages=(ws,es)}) ->
        let err' = mkLocErrMsg loc msg
            es'  = es `snocBag` err'
        in POk s{messages=(ws,es')} ()

getMessages :: PState -> Messages
getMessages PState{messages=ms} = ms

-- -------------------------------------------------------------------
-- Construct a parse error

-- A lexical error is reported at a particular position
-- in the source file, not over a token range.
lexError :: String -> P a
lexError str = do
    (AI loc buf _) <- getInput
    reportLexError loc buf str

-- -------------------------------------------------------------------
-- This is the top-level functions: lexer is called from the parser
-- each time a new token is to be read from the input.

lexToken :: P (Located Token)
lexToken = do
    inp@(AI loc buf _) <- getInput
    sc <- getLexState
    case alexScan inp sc of
        AlexEOF -> do
            if sc > 0
               then errorThen "Probably unmatched open comment symbol"
                        (\_ _ _ -> return (L loc ITeof)) loc buf 0
               else return (L loc ITeof)
        AlexError (AI loc2 buf2 _) ->
            reportLexError loc2 buf2 "lexical error"
        AlexSkip inp2 _ -> do
            setInput inp2
            lexToken
        AlexToken inp2 len t -> do
            setInput inp2
            t loc buf len

reportLexError :: SrcLoc -> String -> String -> P a
reportLexError loc buf str
    | null buf  = failLocMsgP loc (str ++ " at end of input")
    | otherwise =
        let c = head buf
        in
        failLocMsgP loc (str ++ " at character " ++ show c)

lexer :: (Located Token -> P a) -> P a
lexer cont = do
  tok@(L _span _tok__) <- lexToken
  cont tok

-- used for debugging as it not contains continuation function
--
lexDummy :: P [(Located Token)]
lexDummy = do
    tok@(L _pos t) <- lexToken
    if t==ITeof
        then do let toks = []
                P $ \s -> POk s (tok : toks)
        else do toks <- lexDummy
                P $ \s -> POk s (tok : toks)

}
