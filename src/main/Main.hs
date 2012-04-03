--------------------------------------------------------------------------------
--
-- GAC Driver program
--
-- (c) Tsitsimpis Ilias, 2011-2012
--
--------------------------------------------------------------------------------

module Main(main) where

#include "config.h"

import Lexer
import Parser
import SrcLoc
import Outputable
import TcMonad
import SymbolTable
import TypeCheck
import ErrUtils
import UnTypedAst
import TypedAst (TAst)
import DynFlags
import ModeFlags
import SysTools
import LambdaLift
import LlvmCodeGen

import System.Exit
import System.Environment
import System.FilePath
import Data.List
import Data.Maybe
import Control.Monad (when)
import qualified Data.ByteString as BS
import LLVM.Core


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
                               main' postLoadMode dflags1 argv2

-- ---------------------------
-- Parse DynFlags
main' :: PostLoadMode -> DynFlags -> [Located String] -> IO ()
main' postLoadMode dflags0 args = do
    -- Parse the "dynamic" arguments
    -- Leftover ones are presumably files
    case parseDynamicFlags dflags0 args of
         Left errs -> do
             printLocErrs errs
             exitFailure
         Right (dflags1, fileish_args, dynamicFlagWarnings) -> do
             printLocWarns dynamicFlagWarnings
             let normal_fileish_paths = map (normalise . unLoc) fileish_args
             (srcs, asms, objs) <- partition_args normal_fileish_paths [] [] []
             case length srcs of
                  0 -> do
                      printErrs [ progName ++ ": no input files", usageString ]
                      exitFailure
                  1 -> main'' postLoadMode dflags1 srcs asms objs
                  _ -> do
                      if xopt Opt_ExplicitMain dflags1
                         then main'' postLoadMode dflags1 srcs asms objs
                         else do
                             printErrs [ progName ++ ": cannot handle multiple source files"
                                       , "Use -XExplicitMain if you want to enable this" ]
                             exitFailure

-- ---------------------------
-- Right now handle only one file
main'' :: PostLoadMode -> DynFlags -> [String] -> [String] -> [String] -> IO ()
main'' postLoadMode dflags srcs asms objs = do
    -- firstly compile our source files into assembly code
    src_asms <- mapM (driverParse dflags) srcs
    case postLoadMode of
         StopBeforeAs -> do
             cleanAndExit True dflags
         StopBeforeLn -> do
             -- ------------------
             -- Run the assembler
             let asms' = (reverse .catMaybes) src_asms ++ asms
             src_objs <- mapM (driverAssemble dflags) asms'
             let objs' = src_objs ++ objs
             if isNoLink (gacLink dflags) || null objs'
                then cleanAndExit True dflags
                else do
                    -- -------------------------
                    -- Link into one executable
                    driverLink dflags objs'
                    cleanAndExit True dflags


-- -------------------------------------------------------------------
-- Drive one source file through all the necessary compilation steps

-- ---------------------------
-- parse a file and call the typechecker
driverParse :: DynFlags -> String -> IO (Maybe String)
driverParse dflags filename = do
    let out_file = case outputDir dflags of
                        Just od -> replaceDirectory filename od
                        Nothing -> filename
    contents <- BS.readFile filename
    let p_state = mkPState dflags contents (mkSrcLoc filename 1 1)
    case unP parser p_state of
         PFailed msg        -> do
             printMessages dflags msg
             cleanAndExit False dflags
         POk p_state' luast -> do
             when (dopt Opt_D_dump_parsed dflags) $
                 printDumpedAst (dopt Opt_DumpToFile dflags)
                    (replaceExtension out_file "dump-parsed") (unLoc luast)
             let p_messages = getPMessages p_state'
             if errorsFound p_messages ||
                 (warnsFound p_messages && dopt Opt_WarnIsError dflags)
                then do
                    printMessages dflags p_messages
                    cleanAndExit False dflags
                else do
                    driverTypeCheck dflags p_messages out_file luast

-- ---------------------------
-- type check an UAst and call the codeGenerator
-- the produced object file (if any)
driverTypeCheck :: DynFlags -> Messages -> String -> (Located UAst) -> IO (Maybe String)
driverTypeCheck dflags p_messages out_file luast = do
    let tc_state = mkTcState dflags predefinedTable
    case unTcM (typeCheckAst luast) tc_state of
         TcFailed msg         -> do
             printMessages dflags msg
             cleanAndExit False dflags
         TcOk tc_state' tast -> do
             let tc_messages  = (getTcMessages tc_state')
                 tc_messages' = unionMessages p_messages tc_messages
             printMessages dflags tc_messages'
             if errorsFound tc_messages' ||
                 (warnsFound tc_messages && dopt Opt_WarnIsError dflags)
                then do
                    -- errors found
                    cleanAndExit False dflags
                else do
                    -- check for `-fno-code'
                    if isObjectTarget $ alcTarget dflags
                       then do
                           let protos = getTcProtos tc_state'
                           driverCodeGen dflags out_file protos tast
                       else do
                           return Nothing

-- ---------------------------
-- generate llvm code and return
-- the produced object file (if any)
driverCodeGen :: DynFlags -> String -> [Prototype] -> TAst -> IO (Maybe String)
driverCodeGen dflags out_file protos tast = do
    let tast' = lambdaLift protos tast
    llvm_module <- newModule
    defineModule llvm_module (compile tast')
    -- -----------------------
    -- output llvm file
    let bc_file = replaceExtension out_file "bc"
    when (not $ dopt Opt_KeepLlvmFiles dflags) $
        addFilesToClean dflags [bc_file]
    writeBitcodeToFile bc_file llvm_module
    -- -----------------------
    -- optimize bytecode llvm
    -- don't specify anything if user has specified commands. We do this for
    -- opt but not llc since opt is very specifically for optimisation passes
    -- only, so if the user is passing us extra options we assume they know
    -- what they are doing and don't get in the way.
    let lo_opts  = getOpts dflags opt_lo
        opt_lvl  = optLevel dflags
        opt_Opts = ["", "-O1", "-O2", "-O3"]
        optFlag  = if null lo_opts
                     then [Option (opt_Opts !! opt_lvl)]
                     else []
    runLlvmOpt dflags
        ([FileOption "" bc_file,
            Option "-o",
            FileOption "" bc_file]
        ++ optFlag
        ++ map Option lo_opts)
    -- -----------------------
    -- generate assembly
    let asm_file = replaceExtension out_file "s"
    when (not $ dopt Opt_KeepSFiles dflags) $
        addFilesToClean dflags [asm_file]
    let lc_opts  = getOpts dflags opt_lc
        llc_Opts = ["-O0", "-O1", "-O2", "-O3"]
    runLlvmLlc dflags
        ([Option (llc_Opts !! opt_lvl),
            FileOption "" bc_file,
            Option "-o", FileOption "" asm_file]
        ++ map Option lc_opts)
    return (Just asm_file)


-- ---------------------------
-- generate assembly code
-- return the produced asm file
driverAssemble :: DynFlags -> String -> IO String
driverAssemble dflags input_file = do
    let output_file = replaceExtension input_file "o"
        as_opts = getOpts dflags opt_a
    when (not $ dopt Opt_KeepObjFiles dflags) $
        addFilesToClean dflags [output_file]
    runAs dflags
        (map Option as_opts
        ++ [ Option "-c"
           , FileOption "" input_file
           , Option "-o"
           , FileOption "" output_file ])
    return output_file

-- ---------------------------
-- generate executable
driverLink :: DynFlags -> [String] -> IO ()
driverLink dflags input_files = do
    let verb = getVerbFlag dflags
        out_file = exeFileName dflags
        lib_paths = (topDir dflags) : (libraryPaths dflags)
        lib_paths_opts = map ("-L"++) lib_paths
        extra_ld_opts = getOpts dflags opt_l
    runLink dflags (
        [ Option verb
        , Option "-o"
        , FileOption "" out_file
        ]
        ++ map (FileOption "") input_files
        ++ map Option (lib_paths_opts ++ extra_ld_opts)
        ++ [Option "-lprelude"])
    return ()


-- -------------------------------------------------------------------
-- Clean temp files and exit
cleanAndExit :: Bool -> DynFlags -> IO a
cleanAndExit is_success dflags = do
    cleanTempFiles dflags
    if is_success
       then exitSuccess
       else exitFailure

-- -------------------------------------------------------------------
-- Splitting arguments into source files, assembly files and object files.

partition_args :: [FilePath] -> [String] -> [String] -> [String]
               -> IO ([String], [String], [String])
partition_args [] srcs asms objs = return (reverse srcs, reverse asms, reverse objs)
partition_args (arg:args) srcs asms objs
  | looks_like_an_input arg = partition_args args (arg:srcs) asms objs
  | looks_like_an_asm   arg = partition_args args srcs (arg:asms) objs
  | looks_like_an_obj   arg = partition_args args srcs asms (arg:objs)
  | otherwise = do
      printErrs [ progName ++ ": unrecognised flag `" ++ arg ++ "'"
                , usageString ]
      exitFailure

-- ---------------------------
-- We split out the object files (.o, etc) and add them to
-- v_Ld_inputs for use by the linker.
-- The following things should be considered compilation manager inputs:
--      - alan source files (string ending in .alan extension)
looks_like_an_input :: String -> Bool
looks_like_an_input m = (drop 1 $ takeExtension m) == "alan"

looks_like_an_asm :: String -> Bool
looks_like_an_asm m = (drop 1 $ takeExtension m) == "s"

looks_like_an_obj :: String -> Bool
looks_like_an_obj m = (drop 1 $ takeExtension m) == "o"

-- ---------------------------
exeFileName :: DynFlags -> FilePath
exeFileName dflags =
    case (outputFile dflags, outputDir dflags) of
         (Just s, Just d) -> d </> s
         (Just s, Nothing) -> s
         (Nothing, Just d) -> d </> "a.out"
         (Nothing, Nothing) -> "a.out"
