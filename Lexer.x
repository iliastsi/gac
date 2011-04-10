{
module Lexer where
}

%wrapper "posn"

$digit = 0-9
$white = [\ \t\r]


-- ---------------------------------------------------------------------
-- \p s x -> p for position, s for input string, x for current run state
tokens :-

    $white+       ;
    \n+           { \p s x -> (T_NewLine, x) }
    $digit+       { \p s x -> (T_Int (read $ s), x) }
    "+"           { \p s x -> (T_Plus, x) }
    "-"           { \p s x -> (T_Minus, x) }
    "*"           { \p s x -> (T_Mul, x) }
    "/"           { \p s x -> (T_Div, x) }
    "("           { \p s x -> (T_L, x) }
    ")"           { \p s x -> (T_R, x) }
    <0>.          { \p s x -> (T_ERROR, first) }
    <first>.      { \p s x -> (T_SKIP, 0) }



{

data Token
  = T_Int Float
  | T_Plus
  | T_Minus
  | T_Mul
  | T_Div
  | T_L
  | T_R
  | T_NewLine
  | T_EOF
  | T_ERROR
  | T_SKIP
  deriving (Eq, Show)


-- ------------------------------------------------------------------
-- Functions

-- This is the main Lexer function
lexer :: (Token -> P a) -> P a
lexer cont = P $ \inp@(pos@(AlexPn a l c),_,str) sc ->
  case alexScan inp sc of
    AlexEOF -> runParser (cont T_EOF) inp sc
    AlexError _ -> error (lexError "lexical error" str pos)
    AlexSkip inp' len -> runParser (lexer cont) inp' sc
    AlexToken inp' len act ->
      case act pos (take len str) 0 of
        (T_SKIP, new_sc)  -> runParser (lexer cont) inp' new_sc
        (T_ERROR, new_sc) ->
            let (t1,t2) = runParser (lexer cont) inp' new_sc
            in
            (t1,(lexWarning ("Unknown char " ++ (take 1 str)) pos):t2)
        (tok, new_sc)     -> runParser (cont tok) inp' new_sc

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


-- ------------------------------------------------------------------
-- Define P monad used by our monadic lexer-parser
-- This is the same monad as Writer

type StartCode = Int

newtype P a = P { runParser :: AlexInput -> StartCode -> (a, [String]) }

instance Monad P where
  m >>= k = P $ \inp sc ->
    let (x, v)  = runParser m inp sc
        (y, v') = runParser (k x) inp sc
    in
    (y, v ++ v')
  return a = P $ \inp sc -> (a, [])

}
