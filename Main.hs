module Main(main) where

import System.IO
import Lexer
import Parser

main = do
    s <- getLine
    let tokens = extractLex $ lexer ( s ++ "\n" )
--    p <- parser tokens
    print $ (runParser $ parser tokens) "foo"
    main

printLex :: Either String ([Token], [String]) -> IO ()
printLex (Left msg) = print msg
printLex (Right (t,w)) = do print t; print w

extractLex :: Either String ([Token], [String]) -> [Token]
extractLex (Left msg) = error msg
extractLex (Right (t, w)) = t
