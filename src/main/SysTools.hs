--------------------------------------------------------------------------------
-- (c) Tsitsimpis Ilias, 2011-2012
--
-- Access to system tools: gcc, cp, rm etc
--
-- How GAC finds its files
--
-- Gac needs various support files (library packages etc), plus
-- various auxiliary programs (cp, gcc, etc). It starts by finding topdir,
-- the root of GAC's support files
--      - right now gac always has a shell wrapper that passes a -B<dir> option
--
--
-- SysTools.initSysProgs figures out exactly where all the auxiliary programs
-- are, and initialises mutable variables to make it easy to call them.
--
--------------------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module SysTools (
    -- initialisation
    initSysTools,

    -- Interface to system tools
    runAs, runLink,     -- [Option] -> IO ()
    runLlvmOpt,
    runLlvmLlc,

    copy,
    copyWithHeader,

    -- Temporary-file management
    cleanTempFiles, cleanTempFilesExcept,
    addFilesToClean,

    Option(..)
  ) where

import DynFlags
import Outputable (panic)
import SrcLoc
import ErrUtils
import Util

import Data.IORef
import Control.Exception as Exception
import System.Exit
import System.Environment
import System.FilePath
import System.IO
import System.IO.Error as IO
import System.Directory
import Data.Char
import Data.List
import System.Process (runInteractiveProcess, getProcessExitCode)
import Control.Concurrent


-- -------------------------------------------------------------------
-- Initialisation

initSysTools :: Maybe String    -- Maybe TopDir path (without the '-B' prefix)
             -> DynFlags
             -> IO DynFlags     -- Set all the mutable variables above, holding
                                -- the system programs
initSysTools mbMinusB dflags0 = do
    top_dir <- findTopDir mbMinusB
    -- TODO: actually figure out this program locations
    let gcc_prog   = "gcc"
        as_prog    = gcc_prog
        ld_prog    = gcc_prog
        lc_prog    = "llc"
        lo_prog    = "opt"
    return dflags0
        { topDir  = top_dir
        , pgm_a   = (as_prog,[])
        , pgm_l   = (ld_prog,[])
        , pgm_lo  = (lo_prog,[])
        , pgm_lc  = (lc_prog,[])
        }

-- returns a Unix-format path (relying on getBaseDir to do so too)
findTopDir :: Maybe String  -- Maybe TopDir path (without the '-B' preofix).
           -> IO String     -- TopDir (in Unix format '/' separated)
findTopDir (Just minusb) = return (normalise minusb)
findTopDir Nothing = do
    maybe_exec_dir <- getBaseDir
    case maybe_exec_dir of
         Nothing  -> do
             putStrLn "InstallationError: missing -B<dir> option"
             exitFailure
         Just dir -> return dir


-- -------------------------------------------------------------------
-- Running an external program

-- If the -B<dir> option is set, add <dir> to PATH. This works around
-- a bug in gcc on Windows Vista where it can't find its auxiliary
-- binaries
getGccEnv :: [Option] -> IO (Maybe [(String,String)])
getGccEnv opts =
    if null b_dirs
       then return Nothing
       else do env <- getEnvironment
               return (Just (map mangle_path env))
    where
        (b_dirs, _) = partitionWith get_b_opt opts

        get_b_opt (Option ('-':'B':dir)) = Left dir
        get_b_opt other = Right other

        mangle_path (path,paths) | map toUpper path == "PATH" =
            (path, '\"' : head b_dirs ++ "\";" ++ paths)
        mangle_path other = other

runAs :: DynFlags -> [Option] -> IO Bool
runAs dflags args = do
    let (p,args0) = pgm_a dflags
        args1 = args0 ++ args
    mb_env <- getGccEnv args1
    runSomethingFiltered dflags id "Assembler" p args1 mb_env

runLlvmOpt :: DynFlags -> [Option] -> IO Bool
runLlvmOpt dflags args = do
    let (p,args0) = pgm_lo dflags
    runSomething dflags "LLVM Optimiser" p (args0++args)

runLlvmLlc :: DynFlags -> [Option] -> IO Bool
runLlvmLlc dflags args = do
    let (p,args0) = pgm_lc dflags
    runSomething dflags "LLVM Compiler" p (args0++args)

runLink :: DynFlags -> [Option] -> IO Bool
runLink dflags args = do
    let (p,args0) = pgm_l dflags
        args1 = args0 ++ args
    mb_env <- getGccEnv args1
    runSomethingFiltered dflags id "Linker" p args1 mb_env

copy :: DynFlags -> String -> FilePath -> FilePath -> IO ()
copy dflags purpose from to =
    copyWithHeader dflags purpose Nothing from to

copyWithHeader :: DynFlags -> String -> Maybe String -> FilePath -> FilePath
               -> IO ()
copyWithHeader _dflags _purpose maybe_header from to = do
    hout <- openBinaryFile to   WriteMode
    hin  <- openBinaryFile from ReadMode
    ls <- hGetContents hin  -- inefficient, but it'll do for no. ToDo: speed up
    maybe (return ()) (hPutStr hout) maybe_header
    hPutStr hout ls
    hClose hout
    hClose hin


-- -------------------------------------------------------------------
-- Managing temporary files

cleanTempFiles :: DynFlags -> IO ()
cleanTempFiles dflags = do
    let ref = filesToClean dflags
    fs <- readIORef ref
    _ <- removeTmpFiles dflags fs
    writeIORef ref []

cleanTempFilesExcept :: DynFlags -> [FilePath] -> IO ()
cleanTempFilesExcept dflags dont_delete = do
    let ref = filesToClean dflags
    files <- readIORef ref
    let (to_keep, to_delete) = partition (`elem` dont_delete) files
    _ <- removeTmpFiles dflags to_delete
    writeIORef ref to_keep

addFilesToClean :: DynFlags -> [FilePath] -> IO ()
-- May include wildcards
addFilesToClean dflags files = mapM_ (consIORef (filesToClean dflags)) files

removeTmpFiles :: DynFlags -> [FilePath] -> IO Bool
removeTmpFiles dflags fs =
    traceCmd dflags "Deleting temp files"
            ("Deleting: " ++ unwords deletees)
            (do mapM_ (removeWith dflags removeFile) deletees
                return True)
    where
        (_non_deletees, deletees) = partition isAlanUserSrcFilename fs
        isAlanUserSrcFilename str = (drop 1 $ takeExtension str) == "alan"

removeWith :: DynFlags -> (FilePath -> IO ()) -> FilePath -> IO ()
removeWith _dflags remover f = remover f


-- -------------------------------------------------------------------
-- Running an external program

runSomething :: DynFlags
             -> String      -- For -v message
             -> String      -- Command name (possibly a full path)
             -> [Option]    -- Arguments
             -> IO Bool
runSomething dflags phase_name pgm args =
    runSomethingFiltered dflags id phase_name pgm args Nothing

runSomethingFiltered
  :: DynFlags -> (String->String) -> String -> String -> [Option]
  -> Maybe [(String,String)] -> IO Bool
runSomethingFiltered dflags filter_fn phase_name pgm args mb_env = do
    let real_args = filter notNull (map showOpt args)
    traceCmd dflags phase_name (unwords (pgm:real_args)) $ do
    (exit_code, doesn'tExist) <-
        Exception.catch (do
            rc <- builderMainLoop dflags filter_fn pgm real_args mb_env
            case rc of
                 ExitSuccess{} -> return (rc, False)
                 ExitFailure n
                   -- rawSystem returns (ExitFailure 127) if the exec failed for any
                   -- reason (eq. the program doesn't exist). This is the only clue
                   -- we have, but we need to report something to the user because in
                   -- the case of a missing program there will otherwise be no output
                   -- at all.
                  | n == 127  -> return (rc, True)
                  | otherwise -> return (rc, False))
                      -- Should 'rawSystem' generate an IO exception indicating that
                      -- 'pgm' couldn't be run rather than a funky return code, catch
                      -- this here (the win32 version does this, but it doesn't hurt
                      -- to test for this in general.)
                    (\ err ->
                        if IO.isDoesNotExistError err
                           then return (ExitFailure 1, True)
                           else IO.ioError err)
    case (doesn'tExist, exit_code) of
         (True, _)        -> do
             putStrLn ("Could not execute: " ++ pgm)
             return False
         (_, ExitSuccess) -> do
             return True
         _                -> do
             putStrLn ("Phase `" ++ phase_name ++"' failed with exit code " ++ show exit_code)
             return False

builderMainLoop :: DynFlags -> (String -> String) -> FilePath
                -> [String] -> Maybe [(String, String)]
                -> IO ExitCode
builderMainLoop dflags filter_fn pgm real_args mb_env = do
    chan <- newChan
    (hStdIn, hStdOut, hStdErr, hProcess) <- runInteractiveProcess pgm real_args Nothing mb_env
    -- and run al oop piping the output from the compiler to the log_action in DynFlags
    hSetBuffering hStdOut LineBuffering
    hSetBuffering hStdErr LineBuffering
    _ <- forkIO (readerProc chan hStdOut filter_fn)
    _ <- forkIO (readerProc chan hStdErr filter_fn)
    -- we don't want to finish until w streams have been completed
    -- (stdout and stderr)
    -- nor until 1 exit code has been retrieved.
    rc <- loop chan hProcess (2::Integer) (1::Integer) ExitSuccess
    -- after that, we're done here.
    hClose hStdIn
    hClose hStdOut
    hClose hStdErr
    return rc
    where
        -- status starts at zero, and increments each time either
        -- a reader process gets EOF, or the build proc exits. We wait
        -- for all of these to happen (status==3).
        -- ToDo: we should really have a contingency plan in case any of
        -- the threads dies, such as a timeout.
        loop _    _        0 0 exitcode = return exitcode
        loop chan hProcess t p exitcode = do
            mb_code <- if p > 0
                          then getProcessExitCode hProcess
                          else return Nothing
            case mb_code of
                 Just code -> loop chan hProcess t (p-1) code
                 Nothing
                   | t > 0 -> do
                       msg <- readChan chan
                       case msg of
                            BuildMsg msg -> do
                                log_action dflags SevInfo noSrcSpan msg
                                loop chan hProcess t p exitcode
                            BuildError loc msg -> do
                                log_action dflags SevError (mkSrcSpan loc loc) msg
                                loop chan hProcess t p exitcode
                            EOF ->
                                loop chan hProcess (t-1) p exitcode
                   | otherwise -> loop chan hProcess t p exitcode

readerProc :: Chan BuildMessage -> Handle -> (String -> String) -> IO ()
readerProc chan hdl filter_fn =
    (do str <- hGetContents hdl
        loop (linesPlatform (filter_fn str)) Nothing)
    `finally`
        writeChan chan EOF
        -- ToDo: check errors more carefully
        -- ToDo: in the future, the filter should be implemented as
        -- a stream transformer.
     where
        loop []     Nothing     = return ()
        loop []     (Just err)  = writeChan chan err
        loop (l:ls) (in_err)    =
            case in_err of
                 Just err@(BuildError srcLoc msg)
                   | leading_whitespace l -> do
                       loop ls (Just (BuildError srcLoc (msg ++ l)))
                   | otherwise -> do
                       writeChan chan err
                       checkError l ls
                 Nothing -> do
                     checkError l ls
                 _ -> panic "readerProc/loop"
        checkError l ls =
            case parseError l of
                 Nothing -> do
                     writeChan chan (BuildMsg l)
                     loop ls Nothing
                 Just (file, lineNum, colNum, msg) -> do
                     let srcLoc = mkSrcLoc file lineNum colNum
                     loop ls (Just (BuildError srcLoc msg))
        leading_whitespace [] = False
        leading_whitespace (x:_) = isSpace x

parseError :: String -> Maybe (String, Int, Int, String)
parseError s0 =
    case breakColon s0 of
         Just (filename, s1) ->
             case breakIntColon s1 of
                  Just (lineNum, s2) ->
                      case breakIntColon s2 of
                           Just (columnNum, s3) ->
                               Just (filename, lineNum, columnNum, s3)
                           Nothing ->
                               Just (filename, lineNum, 0, s2)
                  Nothing -> Nothing
         Nothing -> Nothing

breakColon :: String -> Maybe (String, String)
breakColon xs =
    case break (':' ==) xs of
         (ys, _:zs) -> Just (ys, zs)
         _ -> Nothing

breakIntColon :: String -> Maybe (Int, String)
breakIntColon xs =
    case break (':' ==) xs of
         (ys, _:zs)
          | not (null ys) && all isAscii ys && all isDigit ys ->
              Just (read ys, zs)
         _ -> Nothing

data BuildMessage
    = BuildMsg      !String
    | BuildError    !SrcLoc !String
    | EOF

traceCmd :: DynFlags -> String -> String -> IO Bool -> IO Bool
-- trace the command (at two levels of verbosity)
traceCmd _dflags _phase_name _cmd_line action = do
    hFlush stderr
    action


-- -------------------------------------------------------------------
-- Support code

getBaseDir :: IO (Maybe String)
-- Assuming we are running gac, accessed by path $(stuff)/bin/gac,
-- return the path $(stuff)/lib.
getBaseDir = return Nothing

-- Divvy up text stram into lines, taking platform dependent
-- line termination into accound.
linesPlatform :: String -> [String]
#if !defined(mingw32_HOST_OS)
linesPlatform ls = lines ls
#else
linesPlatform "" = []
linesPlatform xs =
    case lineBreak xs of
         (as,xs1) -> as : linesPlatform xs1
    where
        lineBreak "" = ("","")
        lineBreak ('\r':'\r':xs) = ([],xs)
        lineBreak ('\n':xs) = ([],xs)
        lineBreak (x:xs) = let (as,bs) = lineBreak xs in (x:as,bs)
#endif
