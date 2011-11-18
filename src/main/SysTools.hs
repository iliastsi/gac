--------------------------------------------------------------------------------
-- (c) Tsitsimpis Ilias, 2011
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

module SysTools (
    -- initialisation
    initSysTools,

    -- Interface to system tools
    runAs, runLink,     -- [Option] -> IO ()
    runLlvmOpt,
    runLlvmLlc,

    touch,              -- String -> String -> IO ()
    copy,
    copyWithHeader,

    -- Temporary-file management
    setTmpDir,
    newTempName,
    cleanTempDirs, cleanTempFiles, cleanTempFilesExcept,
    addFilesToClean,

    Option(..)
  ) where

import DynFlags
import Outputable (panic)
import SrcLoc
import ErrUtils
import Util

import Data.IORef
import Control.Monad
import Control.Exception
import System.Exit
import System.Environment
import System.FilePath
import System.IO
import System.IO.Error as IO
import System.Directory
import Data.Char
import Data.List
import qualified Data.Map as Map
import qualified System.Posix.Internals
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
    tmpdir <- getTemporaryDirectory
    let dflags1 = setTmpDir tmpdir dflags0
    -- TODO: actually figure out this program locations
    let gcc_prog   = "gcc"
        touch_path = "touch"
        as_prog    = gcc_prog
        ld_prog    = gcc_prog
        lc_prog    = "llc"
        lo_prog    = "opt"
    return dflags1
        { topDir  = top_dir
        , pgm_a   = (as_prog,[])
        , pgm_l   = (ld_prog,[])
        , pgm_lo  = (lo_prog,[])
        , pgm_lc  = (lc_prog,[])
        , pgm_T   = touch_path
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

runAs :: DynFlags -> [Option] -> IO ()
runAs dflags args = do
    let (p,args0) = pgm_a dflags
        args1 = args0 ++ args
    mb_env <- getGccEnv args1
    runSomethingFiltered dflags id "Assembler" p args1 mb_env

runLlvmOpt :: DynFlags -> [Option] -> IO ()
runLlvmOpt dflags args = do
    let (p,args0) = pgm_lo dflags
    runSomething dflags "LLVM Optimiser" p (args0++args)

runLlvmLlc :: DynFlags -> [Option] -> IO ()
runLlvmLlc dflags args = do
    let (p,args0) = pgm_lc dflags
    runSomething dflags "LLVM Compiler" p (args0++args)

runLink :: DynFlags -> [Option] -> IO ()
runLink dflags args = do
    let (p,args0) = pgm_l dflags
        args1 = args0 ++ args
    mb_env <- getGccEnv args1
    runSomethingFiltered dflags id "Linker" p args1 mb_env

touch :: DynFlags -> String -> String -> IO ()
touch dflags purpose arg =
    runSomething dflags purpose (pgm_T dflags) [FileOption "" arg]

copy :: DynFlags -> String -> FilePath -> FilePath -> IO ()
copy dflags purpose from to =
    copyWithHeader dflags purpose Nothing from to

copyWithHeader :: DynFlags -> String -> Maybe String -> FilePath -> FilePath
               -> IO ()
copyWithHeader dflags purpose maybe_header from to = do
    hout <- openBinaryFile to   WriteMode
    hin  <- openBinaryFile from ReadMode
    ls <- hGetContents hin  -- inefficient, but it'll do for no. ToDo: speed up
    maybe (return ()) (hPutStr hout) maybe_header
    hPutStr hout ls
    hClose hout
    hClose hin


-- -------------------------------------------------------------------
-- Managing temporary files

cleanTempDirs :: DynFlags -> IO ()
cleanTempDirs dflags =
    unless (dopt Opt_KeepTmpFiles dflags)
        $ do let ref = dirsToClean dflags
             ds <- readIORef ref
             removeTmpDirs dflags (Map.elems ds)
             writeIORef ref Map.empty

cleanTempFiles :: DynFlags -> IO ()
cleanTempFiles dflags =
    unless (dopt Opt_KeepTmpFiles dflags)
        $ do let ref = filesToClean dflags
             fs <- readIORef ref
             removeTmpFiles dflags fs
             writeIORef ref []

cleanTempFilesExcept :: DynFlags -> [FilePath] -> IO ()
cleanTempFilesExcept dflags dont_delete =
    unless (dopt Opt_KeepTmpFiles dflags)
        $ do let ref = filesToClean dflags
             files <- readIORef ref
             let (to_keep, to_delete) = partition (`elem` dont_delete) files
             removeTmpFiles dflags to_delete
             writeIORef ref to_keep

-- find a temporary name that doesn't already exist
newTempName :: DynFlags -> String -> IO FilePath
newTempName dflags extn = do
    d <- getTempDir dflags
    x <- getProcessID
    findTempName (d </> "gac" ++ show x ++ "_") 0
  where
    findTempName :: FilePath -> Integer -> IO FilePath
    findTempName prefix x = do
        let filename = (prefix ++ show x) <.> extn
        b <- doesFileExist filename
        if b then findTempName prefix (x+1)
             else do --clean it up later
                     consIORef (filesToClean dflags) filename
                     return filename

-- return our temporary directory within tmp_dir, creating one if we
-- don't have one yet
getTempDir :: DynFlags -> IO FilePath
getTempDir dflags@(DynFlags{tmpDir=tmp_dir}) = do
    let ref = dirsToClean dflags
    mapping <- readIORef ref
    case Map.lookup tmp_dir mapping of
         Nothing -> do
             x <- getProcessID
             let prefix = tmp_dir </> "gac" ++ show x ++ "_"
                 mkTempDir :: Integer -> IO FilePath
                 mkTempDir x =
                     let dirname = prefix ++ show x
                     in do
                         createDirectory dirname
                         let mapping' = Map.insert tmp_dir dirname mapping
                         writeIORef ref mapping'
                         return dirname
                      `IO.catch` \e ->
                          if isAlreadyExistsError e
                             then mkTempDir (x+1)
                             else ioError e
             mkTempDir 0
         Just d -> return d

addFilesToClean :: DynFlags -> [FilePath] -> IO ()
-- May include wildcards
addFilesToClean dflags files = mapM_ (consIORef (filesToClean dflags)) files

removeTmpDirs :: DynFlags -> [FilePath] -> IO ()
removeTmpDirs dflags ds =
    traceCmd dflags "Deleting temp dirs"
            ("Deleting: " ++ unwords ds)
            (mapM_ (removeWith dflags removeDirectory) ds)

removeTmpFiles :: DynFlags -> [FilePath] -> IO ()
removeTmpFiles dflags fs =
    traceCmd dflags "Deleting temp files"
            ("Deleting: " ++ unwords deletees)
            (mapM_ (removeWith dflags removeFile) deletees)
    where
        (non_deletees, deletees) = partition isAlanUserSrcFilename fs
        isAlanUserSrcFilename str = (drop 1 $ takeExtension str) == "alan"

removeWith :: DynFlags -> (FilePath -> IO ()) -> FilePath -> IO ()
removeWith dflags remover f = remover f


-- -------------------------------------------------------------------
-- Running an external program

runSomething :: DynFlags
             -> String      -- For -v message
             -> String      -- Command name (possibly a full path)
             -> [Option]    -- Arguments
             -> IO ()
runSomething dflags phase_name pgm args =
    runSomethingFiltered dflags id phase_name pgm args Nothing

runSomethingFiltered
  :: DynFlags -> (String->String) -> String -> String -> [Option]
  -> Maybe [(String,String)] -> IO ()
runSomethingFiltered dflags filter_fn phase_name pgm args mb_env = do
    let real_args = filter notNull (map showOpt args)
    traceCmd dflags phase_name (unwords (pgm:real_args)) $ do
    (exit_code, doesn'tExist) <-
        IO.catch (do
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
         (True, _)        -> putStrLn ("Could not execute: " ++ pgm)
         (_, ExitSuccess) -> return ()
         _                -> putStrLn ("Phase `" ++ phase_name ++"' failed with exit code " ++ show exit_code)

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

traceCmd :: DynFlags -> String -> String -> IO () -> IO ()
-- a) trace the command (at two levels of verbosity)
-- b) don't do it at all if dry-run is set
traceCmd dflags phase_name cmd_line action = do
    let verb = verbosity dflags
    hFlush stderr

    -- Test for -n flag
    unless (dopt Opt_DryRun dflags) action


-- -------------------------------------------------------------------
-- Support code

getBaseDir :: IO (Maybe String)
-- Assuming we are running gac, accessed by path $(stuff)/bin/gac,
-- return the path $(stuff)/lib.
getBaseDir = return Nothing

getProcessID :: IO Int
getProcessID = System.Posix.Internals.c_getpid >>= return . fromIntegral

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
