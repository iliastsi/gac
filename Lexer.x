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
--    .             { lexWarning "Unknown Char" }



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
  | T_WARN String
  deriving (Eq, Show)


-- ------------------------------------------------------------------
-- Functions

-- This is the main Lexer function
lexer :: (Token -> P a) -> P a
lexer cont = P $ \inp@(pos@(AlexPn a l c),_,str) ->
  case alexScan inp 0 of
    AlexEOF -> runParser (cont T_EOF) inp
--    AlexError _ -> error (lexError "lexical error" str pos)
    AlexError (x',y',(z:zs)) -> let (x,v) = runParser (lexer cont) (x',y',zs) in (x,"My lex Error":v)
    AlexSkip inp' len -> runParser (lexer cont) inp'
    AlexToken inp' len act ->
      let (tok, new_sc) = act pos (take len str) 0
      in
        runParser (cont tok) inp'

-- An error encountered
lexError :: String -> String -> AlexPosn -> String
lexError msg input p =
  (showPosn p ++ ":  " ++ msg ++
      (if (not (null input))
          then " before " ++ show (head input)
          else " at end of file"))

{-
-- A warning encountered
lexWarning :: String -> AlexInput -> Int -> Alex Token
lexWarning msg (p,_,s) len = do
  let str = take len s
      warn = (showPosn p ++ ":  " ++ msg ++ " in " ++ str)
  return (T_WARN warn)
-}

-- Return line and column
showPosn :: AlexPosn -> String
showPosn (AlexPn _ line col) = show line ++ ':' : show col


-- ------------------------------------------------------------------
-- Define P monad used by our monadic lexer-parser
-- This is the same monad as Writer

type StartCode = Int

newtype P a = P { runParser :: AlexInput -> (a, [String]) }

instance Monad P where
  m >>= k = P $ \inp ->
    let (x, v)  = runParser m inp
        (y, v') = runParser (k x) inp
    in
    (y, v ++ v')
  return a = P $ \inp -> (a, [])

}
