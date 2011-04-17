module Main(main) where

import System.IO
import Lexer
import Parser

main = do
    str <- getContents
    print $ runP parser (alexStartPos,'\n',str) lexStartState
