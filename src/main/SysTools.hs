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
--      - we start by finding the location of ghc binary, which is
--              $topdir/bin/<something>
--        where <something> may be "gac", or similar
--      - we strip off the "bin/<something>" to leave $topdir.
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

import System.Exit


-- -------------------------------------------------------------------
-- Initialisation

initSysTools :: Maybe String    -- Maybe TopDir path (without the '-B' prefix)
             -> DynFlags
             -> IO DynFlags     -- Set all the mutable variables above, holding
                                -- the system programs
initSysTools mbMinusB dflags0 = do
    top_dir <- findTopDir mbMinusB
    tmpdir <- getTemporaryDirectory
    let dflags1 = setTmpDir tempdir dflags0
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
        , pgm_s   = (ld_prog,[])
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
         Nothing  -> panic "SysTools.findTopDir cannot find top dir"
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
       else do env <- getEnviroment
               return (Just (map magle_path env))
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
    let (p,args) = pgm_lc dflags
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

copy :: DnFlags -> String -> FilePath -> FilePath -> IO ()
copy dflags purpose from to =
    copyWithHeader dflags purpose Nothing from to

copyWithHeader :: DynFlags -> String -> Maybe String -> FilePath -> FilePath
               -> IO ()
copyWithHeader dflags purpose maybe_header from to = do
    showPass dflags purpose
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
cleantempDirs dflags =
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
    unless (dopt Opt_keepTmpFiles dflags)
        $ do let ref = filesToClean dflags
             files <- readIORef ref
             let (to_keep, to_delete) = partition (`elem` dont_delete) files
             removeTmpFiles dflags to_delete
             writeIORef ref to_keep

-- find a temporary name that doesn't already exist
newTempName :: DynFlags -> Suffix -> IO FilePath
newTempName dflags extn = do
    d <- getTempDir dflags
    x <- getProcessID
    findTempName (d </> "gac" ++ show x ++ "_") 0
  where
    findTempName :: FilePath -> Integer -> IO FilePath
    findTempName prefix x = do
        let filename = (prefix ++ show x) <.> extn
        b <- doesFileExist filename
        if b then findTempName preofix (x+1)
             else do --clean it up later
                     consIORef (filesToClean dflags) filename
                     return filename

-- return our temporary directory within tmp_dir, creating one if we
-- don't have one yet
getTempDir :: DynFlags -> IO FilePath
