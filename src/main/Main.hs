--------------------------------------------------------------------------------
--
-- GAC Driver program
--
-- (c) Tsitsimpis Ilias, 2011
--
--------------------------------------------------------------------------------

module Main(main) where

#include "versions.h"

import Lexer (ParseResult(..), mkPState, unP, getPMessages)
import Parser (parser)
import SrcLoc
import Outputable
import TcMonad (TcResult(..), mkTcState, unTcM, getTcMessages)
import SymbolTable (predefinedTable)
import TypeCheck (typeCheckDef)
import ErrUtils
import UnTypedAst (UAst)
import DynFlags
import ModeFlags
import SysTools

import System.Exit (exitSuccess)
import System
import System.FilePath
import Data.List
import System.IO
import Data.Maybe


-- ---------------------------
-- Parse ModeFlags
main :: IO ()
main = do
    prog_name <- getProgName
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
                               main' prog_name postLoadMode dflags1 argv2

-- ---------------------------
-- Parse DynFlags
main' :: String -> PostLoadMode -> DynFlags -> [Located String] -> IO ()
main' prog_name postLoadMode dflags0 args = do
    -- Parse the "dynamic" arguments
    -- Leftover ones are presumably files
    case parseDynamicFlags dflags0 args of
         Left errs -> do
             printLocErrs errs
             exitFailure
         Right (dflags1, fileish_args, dynamicFlagWarnings) -> do
             printLocWarns dynamicFlagWarnings
             let normal_fileish_paths = map (normalise . unLoc) fileish_args
             (srcs, objs) <- partition_args prog_name normal_fileish_paths [] []
             if (null objs) && (length srcs == 1)
                then do
                    main'' postLoadMode dflags1 srcs objs
                else do
                    printErrs [ prog_name ++ ": you must specify one alan source file"
                              , usageString ]
                    exitFailure

-- ---------------------------
-- Right now handle only one file
main'' :: PostLoadMode -> DynFlags -> [String] -> [String] -> IO ()
main'' postLoadMode dflags0 srcs objs = do
    src_objs <- mapM (driverParse postLoadMode dflags0) srcs
    let _objs' = (reverse . catMaybes) src_objs ++ objs
    exitSuccess


-- -------------------------------------------------------------------
-- Drive one source file through all the necessary compilation steps

-- ---------------------------
-- parse a file and return
-- the produced object file (if any)
driverParse :: PostLoadMode -> DynFlags -> String -> IO (Maybe String)
driverParse postLoadMode dflags filename = do
    handle <- openFile filename ReadMode
    contents <- hGetContents handle
    let p_state = mkPState dflags contents (mkSrcLoc filename 1 1)
    case unP parser p_state of
         PFailed msg        -> do
             hClose handle
             printMessages dflags msg
             exitFailure
         POk p_state' luast -> do
             hClose handle
             if dopt Opt_D_dump_parsed dflags
                then printDumpedAst (unLoc luast)
                else return ()
             let p_messages = getPMessages p_state'
             if errorsFound p_messages ||
                 (warnsFound p_messages && dopt Opt_WarnIsError dflags)
                then do
                    printMessages dflags p_messages
                    exitFailure
                else do
                    driverTypeCheck postLoadMode dflags p_messages luast

-- ---------------------------
-- type check an UAst and return
-- the produced object file (if any)
driverTypeCheck :: PostLoadMode -> DynFlags -> Messages -> (Located UAst) -> IO (Maybe String)
driverTypeCheck _postLoadMode dflags p_messages luast = do
    let tc_state = mkTcState dflags predefinedTable
    case unTcM (typeCheckDef luast) tc_state of
         TcFailed msg         -> do
             printMessages dflags msg
             exitFailure
         TcOk tc_state' _ltast -> do
             let tc_messages  = (getTcMessages tc_state')
                 tc_messages' = unionMessages p_messages tc_messages
             if errorsFound tc_messages' ||
                 (warnsFound tc_messages && dopt Opt_WarnIsError dflags)
                then do
                    printMessages dflags tc_messages'
                    exitFailure
                else do
                    printMessages dflags tc_messages'
                    return Nothing


-- -------------------------------------------------------------------
-- Splitting arguments into source files and object files.

partition_args :: String -> [String] -> [String] -> [String]
               -> IO ([String], [String])
partition_args _ [] srcs objs = return (reverse srcs, reverse objs)
partition_args pname (arg:args) srcs objs
  | looks_like_an_input arg = partition_args pname args (arg:srcs) objs
  | looks_like_an_obj   arg = partition_args pname args srcs (arg:objs)
  | otherwise               = do
      printErrs [ pname ++ ": unrecognised flags: " ++ arg
                , usageString ]
      exitFailure

-- ---------------------------
-- We split out the object files (.o, etc) and add them to
-- v_Ld_inputs for use by the linker.
-- The following things should be considered compilation manager inputs:
--      - alan source files (string ending in .alan extension)
looks_like_an_input :: String -> Bool
looks_like_an_input m = (drop 1 $ takeExtension m) == "alan"

looks_like_an_obj :: String -> Bool
looks_like_an_obj m = (drop 1 $ takeExtension m) == "o"
