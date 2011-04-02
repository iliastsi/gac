{
module Lexer(lexer,Token(..)) where
}

%wrapper "monad"

$digit = 0-9
--$white = [\ \t\r\n]
$new_line = \n

tokens :-

    $white+       { skip }
    $digit+       { mkT T_Int }


{

data Token = Token TokenClass String
             deriving (Show)

data TokenClass = T_Int
                | T_Unknown
                | T_EOF
                  deriving (Eq, Show)


--Functions

--Construct tokens
mkT :: TokenClass -> AlexInput -> Int -> Alex Token
mkT c (_, _, str) len = return (Token c (take len str))

--This is the main Lexer function
lexer :: String -> Either String [Token]
lexer str = runAlex str $ do
  let loop i = do tok@(Token cl _) <- lexMonadScan
                  if cl == T_EOF
                     then return i
                     else do loop $! (tok:i)
  loop []

--Overide alexError to print line and column
lexError :: String -> Alex a
lexError s = do
  (p, c, input) <- alexGetInput
  alexError (showPosn p ++ ":  " ++ s ++
              (if (not (null input))
                  then " before " ++ show (head input)
                  else " at end of file"))

--Return line and column
showPosn :: AlexPosn -> String
showPosn (AlexPn _ line col) = show line ++ ':' : show col

--Overide alexMonadScan to use lexError instead of alexError
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

alexEOF = return (Token T_EOF "")

}
