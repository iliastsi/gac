{
module Lexer(lexer
            ,Token(..)) where
}

%wrapper "monad"

$digit = 0-9
--$white = [\ \t\r\n]

tokens :-

    $white+       { mySkip }
    $digit+       { \(_,_,s) l -> return (T_Int (read $ take l s)) }
    "+"           { \input len -> return T_Plus }
    "-"           { \input len -> return T_Minus }
    "*"           { \input len -> return T_Mul }
    "/"           { \input len -> return T_Div }
    "("           { \input len -> return T_L }
    ")"           { \input len -> return T_R }
    .             { lexWarning "Unknown Char" }



{

data Token = T_Int Int
           | T_Plus
           | T_Minus
           | T_Mul
           | T_Div
           | T_L
           | T_R
           | T_EOF
           | T_WARN String
             deriving (Eq, Show)


-- ------------------------------------------------------------------
-- Functions

-- This is the main Lexer function
lexer :: String -> Either String ([Token], [String])
lexer str = runAlex str loop
  where loop = do tok <- lexMonadScan
                  case tok of
                       T_EOF      -> return ([], [])
                       T_WARN msg -> loop >>= \(t,w) -> return $ (t, msg:w)
                       otherwise  -> loop >>= \(t,w) -> return $ (tok:t,w)

-- An error encountered
lexError :: String -> Alex a
lexError s = do
  (p, c, input) <- alexGetInput
  alexError (showPosn p ++ ":  " ++ s ++
              (if (not (null input))
                  then " before " ++ show (head input)
                  else " at end of file"))

-- A warning encountered
lexWarning :: String -> AlexInput -> Int -> Alex Token
lexWarning msg (p,_,s) len = do
  let str = take len s
      warn = (showPosn p ++ ":  " ++ msg ++ " in " ++ str)
  return (T_WARN warn)

-- Return line and column
showPosn :: AlexPosn -> String
showPosn (AlexPn _ line col) = show line ++ ':' : show col


-- ------------------------------------------------------------------
-- Some functions needed to overide in order to change alexMonadScan

-- Overide alexMonadScan to use lexError instead of alexError
lexMonadScan = do
  inp <- alexGetInput
  sc <- alexGetStartCode
  case alexScan inp sc of
       AlexEOF -> alexEOF
       AlexError inp' -> lexError "lexical error"
       AlexSkip  inp' len -> do
         alexSetInput inp'
         lexMonadScan
       AlexToken inp' len action -> do
         alexSetInput inp'
         action inp len

-- Overide skip to use lexMonadScan instead of alexMonadScan
mySkip input len = lexMonadScan

-- Overide begin to use lexMonadScan instead of alexMonadScan
myBegin code input len = do alexSetStartCode code; lexMonadScan

alexEOF = return T_EOF

}
