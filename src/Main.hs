--------------------------------------------------------------------------------
--
-- GAC Driver program
--
-- (c) Tsitsimpis Ilias, 2011
--
--------------------------------------------------------------------------------

module Main(main) where

import System.IO()
import Lexer
import Parser
import SrcLoc

main :: IO ()
main = do
    str <- getContents
    print $ unP parser (mkPState str (mkSrcLoc "Stdin" 1 1))
