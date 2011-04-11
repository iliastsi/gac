{
module Lexer where
}

%wrapper "posn"

$digit = 0-9
$alpha = [a-z A-Z]
$id = [$alpha \_ $digit]
$char = [^\'\"\\]
$hexit = [$digit a-f A-F]

@special = \\n | \\t | \\r | \\0 | \\\\ | \\\' | \\\" | \\x $hexit $hexit


-- ---------------------------------------------------------------------
-- \p s x -> p for position, s for input string, x for current state
tokens :-

    $white+           ;
    <0> byte          { \p s x -> (T_kwByte, x) }
    <0> return        { \p s x -> (T_Return, x) }
    <0> else          { \p s x -> (T_Else, x) }
    <0> while         { \p s x -> (T_While, x) }
    <0> false         { \p s x -> (T_False, x) }
    <0> true          { \p s x -> (T_True, x) }
    <0> if            { \p s x -> (T_If, x) }
    <0> int           { \p s x -> (T_kwInt, x) }
    <0> proc          { \p s x -> (T_Proc, x) }
    <0> reference     { \p s x -> (T_Reference, x) }
    <0> $alpha $id*   { \p s x -> (T_Id s, x) }
    <0> $digit+       { \p s x -> (T_Int (read  s), x) }
    <0> \' ($char|@special) \'
                      { \p s x -> (T_Char s, x) }
    <0> \" ($char|@special)* \"
                      { \p s x -> (T_String s, x) }
    <0> "="           { \p s x -> (T_Assign, x) }
    <0> "+"           { \p s x -> (T_Plus, x) }
    <0> "-"           { \p s x -> (T_Minus, x) }
    <0> "*"           { \p s x -> (T_Times, x) }
    <0> "/"           { \p s x -> (T_Div, x) }
    <0> "%"           { \p s x -> (T_Mod, x) }
    <0> "!"           { \p s x -> (T_Not, x) }
    <0> "&"           { \p s x -> (T_And, x) }
    <0> "|"           { \p s x -> (T_Or, x) }
    <0> "=="          { \p s x -> (T_Eq, x) }
    <0> "!="          { \p s x -> (T_Ne, x) }
    <0> "<"           { \p s x -> (T_Lt, x) }
    <0> ">"           { \p s x -> (T_Gt, x) }
    <0> "<="          { \p s x -> (T_Le, x) }
    <0> ">="          { \p s x -> (T_Ge, x) }
    <0> "("           { \p s x -> (T_Op, x) }
    <0> ")"           { \p s x -> (T_Cp, x) }
    <0> "["           { \p s x -> (T_Os, x) }
    <0> "]"           { \p s x -> (T_Cs, x) }
    <0> "{"           { \p s x -> (T_Oc, x) }
    <0> "}"           { \p s x -> (T_Cc, x) }
    <0> ","           { \p s x -> (T_Comma, x) }
    <0> ":"           { \p s x -> (T_Colon, x) }
    <0> ";"           { \p s x -> (T_SemiColon, x) }
    "(*"              { embedComment }
    <comments> .      ;
    <comments> "*)"   { unembedComment }
    .                 { \p s x -> (T_ERROR ("Unknonw char " ++ s), x) }



{

data Token
  = T_NewLine
  | T_kwByte
  | T_Return
  | T_Else
  | T_While
  | T_False
  | T_True
  | T_If
  | T_kwInt
  | T_Proc
  | T_Reference
  | T_Id String
  | T_Int Float
  | T_Char String
  | T_String String
  | T_Assign
  | T_Plus
  | T_Minus
  | T_Times
  | T_Div
  | T_Mod
  | T_Not
  | T_And
  | T_Or
  | T_Eq
  | T_Ne
  | T_Lt
  | T_Gt
  | T_Le
  | T_Ge
  | T_Op
  | T_Cp
  | T_Os
  | T_Cs
  | T_Oc
  | T_Cc
  | T_Comma
  | T_Colon
  | T_SemiColon
  | T_EOF
  | T_ERROR String
  | T_SKIP
  deriving (Eq, Show)


-- ------------------------------------------------------------------
-- Functions

-- This is the main Lexer function
lexer :: (Token -> P a) -> P a
lexer cont = P $ \inp@(pos@(AlexPn a l c),_,str) state@(sc,nc) ->
  case alexScan inp sc of
    AlexEOF ->
        let (t1,t2) = runP (cont T_EOF) inp state in
          if sc==comments then (t1, (lexWarning "Unclosed comments" pos):t2) else (t1,t2)
    AlexError _ -> error (lexError "lexical error" str pos)
    AlexSkip inp' len -> runP (lexer cont) inp' state
    AlexToken inp' len act ->
      case act pos (take len str) state of
        (T_SKIP, new_state)  -> runP (lexer cont) inp' new_state
        (T_ERROR msg, new_state) ->
            let (t1,t2) = runP (lexer cont) inp' new_state
            in
            (t1, (lexWarning msg pos):t2)
        (tok, new_state)     -> runP (cont tok) inp' new_state

-- An error encountered
lexError :: String -> String -> AlexPosn -> String
lexError msg input p =
  (showPosn p ++ ":  " ++ msg ++
      (if (not (null input))
          then " before " ++ show (head input)
          else " at end of file"))


-- An Unknown Token encountered
lexWarning :: String -> AlexPosn -> String
lexWarning msg pos = (showPosn pos ++ ":  " ++ msg)

-- Return line and column
showPosn :: AlexPosn -> String
showPosn (AlexPn _ line col) = show line ++ ':' : show col


-- Dummy Lexer function to be used by our independent lexer
-- The main lexer function needs a continuation and will be
-- used by our parser
lexDummy :: P [Token]
lexDummy = P $ \inp@(pos@(AlexPn a l c),_,str) state@(sc, nc) ->
  case alexScan inp sc of
    AlexEOF -> if sc==comments then ([],[lexWarning "Unclosed comments" pos]) else ([],[])
    AlexError _ -> error (lexError "lexical error" str pos)
    AlexSkip inp' len -> runP lexDummy inp' state
    AlexToken inp' len act ->
      case act pos (take len str) state of
        (T_SKIP, new_state)  -> runP lexDummy inp' new_state
        (T_ERROR msg, new_state) ->
            let (t1,t2) = runP lexDummy inp' new_state
            in
            (t1, (lexWarning msg pos):t2)
        (tok, new_state)     ->
            let (t1,t2) = runP lexDummy inp' new_state
            in
            (tok:t1, t2)


-- ------------------------------------------------------------------
-- Functions to handle embended comments

embedComment :: AlexPosn -> String -> StateCode -> (Token, StateCode)
embedComment p s (sc, nc) = (T_SKIP, (comments, nc+1))

unembedComment :: AlexPosn -> String -> StateCode -> (Token, StateCode)
unembedComment p s (sc,nc)
  | nc >  1   = (T_SKIP, (comments,nc-1))
  | nc == 1   = (T_SKIP, (0,0))
  | otherwise = (T_ERROR "Unmatch comments close *)", (0,0))


-- ------------------------------------------------------------------
-- Define P monad used by our monadic lexer-parser
-- This is the same monad as Writer

-- StateCode = (StartCode, Number_of_Embedded_comments)
type StateCode = (Int, Int)

newtype P a = P { runP :: AlexInput -> StateCode -> (a, [String]) }

instance Monad P where
  m >>= k = P $ \inp state ->
    let (x, v)  = runP m inp state
        (y, v') = runP (k x) inp state
    in
    (y, v ++ v')
  return a = P $ \inp state -> (a, [])

}
