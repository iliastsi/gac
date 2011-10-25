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
import TcMonad
import SymbolTable
import TypeCheck

main :: IO ()
main = do
    str <- getContents
    case unP parser (mkPState str (mkSrcLoc "Stdin" 1 1)) of
         POk _state ast -> do
             case unTcM (typeCheckDef ast) (mkTcState predefinedTable) of
                  TcOk tcstate _ -> printOutput (getTcMessages tcstate)
                  TcFailed tcm   -> printOutput tcm
         PFailed pm      -> printOutput pm
