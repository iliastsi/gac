module Main(main) where

import System.IO()
import Lexer
import Parser
import SrcLoc

main :: IO ()
main = do
    str <- getContents
    print $ unP parser (mkPState str (SrcLoc 1 1 1))
