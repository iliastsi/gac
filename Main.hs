module Main(main) where

import System.IO
import Lexer

main = do
    s <- getContents
    printLex (lexer s)

printLex :: Either String ([Token], [String]) -> IO ()
printLex (Left msg) = print msg
printLex (Right (t,w)) = do print t; print w
