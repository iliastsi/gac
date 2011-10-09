--------------------------------------------------------------------------------
--
-- GAC Driver program
--
-- (c) Tsitsimpis Ilias, 2011
--
--------------------------------------------------------------------------------

module Main(main) where

import System.IO()
import Lexer (ParseResult(..), mkPState, unP)
import Parser (parser)
import SrcLoc (mkSrcLoc)
import Outputable

main :: IO ()
main = do
    str <- getContents
    case unP parser (mkPState str (mkSrcLoc "Stdin" 1 1)) of
         POk _state ast -> putStrLn "Ok!!"
         PFailed ms     -> printOutput ms
