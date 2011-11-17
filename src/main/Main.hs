--------------------------------------------------------------------------------
--
-- GAC Driver program
--
-- (c) Tsitsimpis Ilias, 2011
--
--------------------------------------------------------------------------------

module Main(main) where

import Lexer (ParseResult(..), mkPState, unP, PState, getPMessages)
import Parser (parser)
import SrcLoc (mkSrcLoc, Located)
import Outputable
import TcMonad (TcResult(..), mkTcState, unTcM, TcState, getTcMessages)
import SymbolTable (predefinedTable)
import TypeCheck (typeCheckDef)
import ErrUtils (errorsFound, unionMessages)
import UnTypedAst (UDef)
import TypedAst (ADef)

import System.Exit (exitSuccess, exitFailure)


parse :: String -> IO (PState, Located UDef)
parse buf = do
    case unP parser (mkPState buf (mkSrcLoc "Stdin" 1 1)) of
         PFailed msg       -> do
             printMessages msg
             exitFailure
         POk p_state luast -> do
             return (p_state, luast)

typeCheck :: (Located UDef) -> IO (TcState, Located ADef)
typeCheck luast = do
    case unTcM (typeCheckDef luast) (mkTcState predefinedTable) of
         TcFailed msg       -> do
             printMessages msg
             exitFailure
         TcOk tc_state ltast -> do
             return (tc_state, ltast)

main :: IO ()
main = do
    str <- getContents
    (p_state, luast) <- parse str
    let p_messages = getPMessages p_state
    if errorsFound p_messages
       then do
           printMessages p_messages
           exitFailure
       else do
           return ()
    (tc_state, _) <- typeCheck luast   
    let tc_messages = unionMessages p_messages (getTcMessages tc_state)
    if errorsFound tc_messages
       then do
           printMessages tc_messages
           exitFailure
       else do
           return ()
    printMessages tc_messages
    exitSuccess
