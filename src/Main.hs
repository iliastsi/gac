module Main(main) where

import System.IO()
import Lexer
import Parser

main :: IO ()
main = do
    str <- getContents
    print $ runP parser (alexStartPos,'\n',str) lexStartState
