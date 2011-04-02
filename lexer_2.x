{
module Lexer(lexer
            ,Token(..)) where
}

%wrapper "monad"

$digit = 0-9
$white = [\ \t\r\n]
$new_line = \n

tokens :-

    $white+       { mySkip }
    $digit+       { \(_,_,s) l -> return (Right (T_Int (read $ take l s))) }
    .             { lexWarning "Unknown Char" }




{

data Token = T_Int Int
           | T_Unknown
           | T_EOF
             deriving (Eq, Show)


-- ------------------------------------------------------------------
-- Functions

-- This is the main Lexer function
lexer :: String -> Either String ([String], [Token])
lexer str = runAlex str loop
  where loop = do scan <- lexMonadScan
                  case scan of
                       Left msg  -> loop >>= \(w,t) -> return $ (msg:w, t)
                       Right tok ->
                         if tok == T_EOF
                            then return ([], [])
                            else loop >>= \(w,t) -> return $ (w,tok:t)

-- An error encountered
lexError :: String -> Alex a
lexError s = do
  (p, c, input) <- alexGetInput
  alexError (showPosn p ++ ":  " ++ s ++
              (if (not (null input))
                  then " before " ++ show (head input)
                  else " at end of file"))

-- A warning encountered
--lexWarning :: String -> AlexInput -> Int -> Alex a
lexWarning msg (p,_,s) len = do
  let str = take len s
      warn = (showPosn p ++ ":  " ++ msg ++ " in " ++ str)
  return (Left warn)

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

alexEOF = return (Right T_EOF)

}
