--------------------------------------------------------------------------------
--
-- GAC Driver program
--
-- (c) Tsitsimpis Ilias, 2011
--
--------------------------------------------------------------------------------

module Main(main) where

#include "versions.h"

import Lexer (ParseResult(..), mkPState, unP, PState, getPMessages)
import Parser (parser)
import SrcLoc
import Outputable
import TcMonad (TcResult(..), mkTcState, unTcM, TcState, getTcMessages)
import SymbolTable (predefinedTable)
import TypeCheck (typeCheckDef)
import ErrUtils (errorsFound, unionMessages)
import UnTypedAst (UDef)
import TypedAst (ADef)
import DynFlags
import ModeFlags
import SysTools

import System.Exit (exitSuccess, exitFailure)
import System
import System.FilePath
import Data.List
import System.IO


-- ---------------------------
-- Parse ModeFlags
main :: IO ()
main = do
    -- 1. extract the -B flag from the args
    argv0 <- getArgs

    let (minusB_args, argv1) = partition ("-B" `isPrefixOf`) argv0
        mbMinusB | null minusB_args = Nothing
                 | otherwise = Just (drop 2 (last minusB_args))
    let argv1' = map (mkGeneralLocated "no the commandline") argv1

    -- 2. Parse the "mode" flags (--help, --info etc)
    case parseModeFlags argv1' of
         Left errs -> do
             printLocErrs errs
             exitFailure
         Right (mode, argv2, modeFlagWarnings) -> do
             printLocWarns modeFlagWarnings
             -- If all we wan to do is something like showing the
             -- version number then do it now, befor we start a GAC
             -- session etc. This makes getting basic information much
             -- more resilient.
             case mode of
                  Left preStartupMode -> do
                      case preStartupMode of
                           ShowVersion             -> showVersion
                           ShowNumVersion          -> putStrLn PROJECT_VERSION
                           ShowSupportedExtensions -> showSupportedExtensions
                           ShowGacUsage            -> showGacUsage
                           Print str               -> putStrLn str
                  Right postStartupMode -> do
                      -- start our GAC session
                      dflags0 <- initDynFlags defaultDynFlags
                      dflags1 <- initSysTools mbMinusB dflags0
                      case postStartupMode of
                           Left preLoadMode -> do
                               case preLoadMode of
                                    ShowInfo            -> showInfo dflags1
                                    PrintWithDynFlags f -> putStrLn (f dflags1)
                           Right postLoadMode -> do
                               main' dflags1 argv2

-- ---------------------------
-- Parse DynFlags
main' :: DynFlags -> [Located String] -> IO ()
main' dflags0 args = do
    -- Parse the "dynamic" arguments
    -- Leftover ones are presumably files
    case parseDynamicFlags dflags0 args of
         Left errs -> do
             printLocErrs errs
             exitFailure
         Right (dflags1, fileish_args, dynamicFlagWarnings) -> do
             printLocWarns dynamicFlagWarnings
             let normal_fileish_paths = map (normalise . unLoc) fileish_args
                 (srcs, objs)         = partition_args normal_fileish_paths [] []
             if (null objs) && (length srcs == 1)
                then do
                    main'' dflags1 srcs objs
                else do
                    printErrs ["You must specify one alan source file"]
                    exitFailure

-- ---------------------------
-- Right now handle only one file
main'' :: DynFlags -> [String] -> [String] -> IO ()
main'' dflags0 srcs objs = do
    handle <- openFile (head srcs) ReadMode
    contents <- hGetContents handle
    (p_state, luast) <- parse dflags0 contents
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


-- -------------------------------------------------------------------
-- Parse and Typecheck

parse :: DynFlags -> String -> IO (PState, Located UDef)
parse dflags buf = do
    case unP parser (mkPState dflags buf (mkSrcLoc "Stdin" 1 1)) of
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


-- -------------------------------------------------------------------
-- Splitting arguments into source files and object files.

partition_args :: [String] -> [String] -> [String]
               -> ([String], [String])
partition_args [] srcs objs = (reverse srcs, reverse objs)
partition_args (arg:args) srcs objs
  | looks_like_an_input arg = partition_args args (arg:srcs) objs
  | otherwise               = partition_args args srcs (arg:objs)

-- ---------------------------
-- We split out the object files (.o, etc) and add them to
-- v_Ld_inputs for use by the linker.
-- The following things should be considered compilation manager inputs:
--      - alan source files (string ending in .alan extension)
looks_like_an_input :: String -> Bool
looks_like_an_input m = (drop 1 $ takeExtension m) == "alan"
