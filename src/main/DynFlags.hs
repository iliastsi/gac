--------------------------------------------------------------------------------
-- (c) Tsitsimpis Ilias, 2011
--
-- Dynamic flags
--
-- Most flags are dynamic flags, which means they can change from
-- compilation to compilation. Dynamic Flags can be set only via
-- command-line at the moment.
--
--------------------------------------------------------------------------------

module DynFlags (
  ) where


-- -------------------------------------------------------------------
-- DynFlags

-- | Enumerates the simple on-of-off dynamic flags
data DynFlag
    -- debugging flags
    = Opt_D_dump_asm
    | Opt_D_dump_llvm
    | Opt_D_dump_parsed
    | Opt_DumpToFile        -- ^ Append dump output to files instead of stdout
    | Opt_D_no_debug_output

    | Opt_WarnIsError       -- -Werror; makes warnings fatal
    | Opt_WarnUnreachableCode
    | Opt_WarnUnusedFunction
    | Opt_WarnUnusedParameter
    | Opt_WarnUnusedResult
    | Opt_WarnUnusedVariable
    | Opt_WarnUnitialized
    | Opt_WarnTypeLimits

    -- optimisation opts
    | Opt_CSE
    | Opt_RegsGraph         -- do graph coloring register allocation

    -- misc opts
    | Opt_ForceRecomp
    | Opt_DryRun            -- does not actually run any external commands
    | Opt_ErrorSpans        -- Include full span info in error messages

    -- temporary flags
    | Opt_AutoLinkPackages

    -- keeping stuff
    | Opt_KeepSFiles
    | Opt_KeepTmpFiles
    | Opt_KeepLlvmFiles

  deriving (Eq, Show)

-- | Contains not only a collection of 'DynFlag's but also a plethora of
-- information relating to the compilation of a single file or GHC session
data DynFlags = DynFlags {
    gacLink             :: GacLink,
    alcTarget           :: AlcTarget,
    alcOutName          :: String,      -- ^ Name of the output file
    verbosity           :: Int,         -- ^ Verbosity level: see Note [Verbosity levels]
    optLevel            :: Int,         -- ^ Optimisation level

    targetPlatform      :: Platform,    -- ^ The platform we're compiling for. Used by the NCG
    importPaths         :: [FilePath],

    -- paths etc
    objectDir           :: Maybe String,
    objectSuf           :: String,
    outputFile          :: Maybe String,

    includePaths        :: [String],
    libraryPaths        :: [String],
    tmpDir              :: String,

    gacUsagePath        :: FilePath,    -- Filled in by SysTools

    -- options for particular phases
    opt_a               :: [String],
    opt_l               :: [String],
    opt_lo              :: [String],    -- LLVM: llvm optimiser
    opt_lc              :: [String],    -- LLVM: llc static compiler

    -- commands for particular phases
    pgm_a               :: (String,[Option]),
    pgm_l               :: (String,[Option]),
    pgm_lo              :: (String,[Option]),   -- LLVM: opt llvm optimiser
    pgm_lc              :: (String,[Option]),   -- LLVM: llc static compiler

    -- Temporary files
    filesToClean        :: [FilePath],
    dirsToClean         :: (Map FilePath FilePath),

    -- gac dynamic flags
    flags               :: [DynFlag]
  }

-- | The target code type of the compilation (if any)
--
-- Whenever you change the target, also make sure to set 'gacLink' to
-- something sensible.
--
-- 'AlcNothing' can be used to aboid generating any output
--
data AlcTarget
    = AlcAsm        -- ^ Generate assembly using the native code generator
    | AlcLlvm       -- ^ Generate assembly using the llvm code generator
    | AlcNothing    -- ^ Don't generate any code
  deriving (Eq, Show)

-- | Will this target result in an object file on the disk?
isObjectTarget :: AlcTarget -> Bool
isObjectTarget AlcAsm   = True
isObjectTarget AlcLlvm  = True
isObjectTarget _        = False

-- | What to do in the link step, if there is one
data GacLink
    = NoLink        -- ^ Don't link at all
    | LinkBinary    -- ^ Link object code into a binary
    | LinkDynLib    -- ^ Link objects into a dynamic lib (DSO on ELF platforms)

isNoLink :: GacLink -> Bool
isNoLink NoLink = True
isNoLink _      = False

defaultAlcTarget :: AlcTarget
defaultAlcTarget = defaultObjectTarget

-- | The 'AlcTarget' value corresponding to the default way to create
-- object files on the current platform
defaultObjectTarget :: AlcTarget
defaultObjectTarget = AlcLlvm

-- | Partially initialize a new 'DynFlags' value
initDynFlags :: DynFlags -> DynFlags
initDynFlags dflags =
    dflags{
        filesToClean    = []
        dirsToClean     = Map.empty
    }

-- | The normal 'DynFlags'. Note that they is not suitable for use in this form
defaultDynFlags :: DynFlags
defaultDynFlags =
    DynFlags {
        gacLink             = LinkBinary,
        alcTarget           = defaultAlcTarget,
        alcOutName          = "",
        verbosity           = 0,
        optLevel            = 0,

        targetPlatform      = defaultTargetPlatform,
        importPaths         = ["."],

        objectDir           = Nothing,
        objectSuf           = "o",

        outputFile          = Nothing,
        includePaths        = [],
        libraryPaths        = [],
        tmpDir              = defaultTmpDir,

        opt_a               = [],
        opt_l               = [],
        opt_lo              = [],
        opt_lc              = [],

        -- initSysTools fills all these in
        pgm_a               = panic "defaultDynFlags: No pgm_a",
        pgm_l               = panic "defaultDynFlags: No pgm_l",
        pgm_lo              = panic "defaultDynFlags: No pgm_lo",
        pgm_lc              = panic "defaultDynFlags: No pgm_lc",
        -- end of initSynTools values
 
        filesToClean        = panic "defaultDynFlags: No filesToClean",
        dirsToClean         = panic "defaultDynFlags: No dirsToClean",

        flags               = defaultFlags
    }

{-
Note [Verbosity levels]
~~~~~~~~~~~~~~~~~~~~~~~
    0   |   print erros & warnings only
    1   |   minimal verbosiy: print "compiling M ... done."
    2   |   equivalent to -dshow-passes
    3   |   equivalent to existing "gac -v"
    4   |   "gac -v -ddump-most"
    5   |   "gac -v -ddump-all"
-}

data OnOff a
    = On  a
    | Off a

-- | Test whether a 'DynFlag' is set
dopt :: DynFlag -> DynFlags -> Bool
dopt f dflags = f `elem` (flags dflags)

-- | Set a 'DynFlag'
dopt_set :: DynFlags -> DynFlag -> DynFlags
dopt_set dfs f = dfs { flags = f : flags dfs }

-- | Unset a 'DynFlag'
dopt_unset :: DynFlags -> DynFlag -> DynFlags
dopt_unset dfs f = dfs{ flags = filter (/= f) (flags dfs) }

-- | Retrive the options corresponding to a particular @opt_*@ filed in the correct order
getOpts :: DynFlags             -- ^ 'DynFlags' to retrieve the options from
        -> (DynFlags -> [a])    -- ^ Relevant record accessor: one of the @opt_*@ accessors
        -> [a]                  -- ^ Correctly ordered extracted options
getOpts dflags opts = reverse (opts dflags)
        -- We add to the options from the front, so we need to reverse the list

-- | Get the verbosity flag for the current verbosity level. This is fed to
-- other tools, so GAC-specific verbosity flags like @-ddump-most@ are not included
getVerbFlag :: DynFlags -> String
getVerFlag dflags
    | verbosity dflags >= 3 = "-v"
    | otherwise = ""

setObjectDir, setObjectSuf, addOptl
    :: String -> DynFlags -> DynFlags
setOutputFile
    :: Maybe String -> DynFlags -> DynFlags
setObjectDir        f d = d{ objectDir = Just f }
setObjectSuf        f d = d{ objectSuf = f}
addOptl             f d = d{ opt_l = f : opt_l d }
setOutputFile       f d = d{ outputFile = f }


-- -------------------------------------------------------------------
-- Command-line options
--
-- | When invoking external tools as part of the compilation pipeline, we
-- pass these a sequence of options on the command-line. Rather than
-- just using a list of Strings, we use a type that allows us to distinguish
-- between filepaths and 'other stuff'. The reason for this is that
-- this type gives us a handle on transforming filenames, and filenames only,
-- to whatever format they're expected to be on a particular platform.
data Option
    = FileOption -- an entry that _contains_ filename(s) / filepaths.
                String  -- a non-filepath prefix that shouldn't be
                        -- transformed (e.g., "/out=")
                String  -- the filepath/filename portion
    | Option    String

showOpt :: Option -> String
showOpt (FileOption pre f) = pre ++ f
showOpt (Option s) = s


-- -------------------------------------------------------------------
-- Setting the optimisation level

updOptLevel :: Int -> DynFlags -> DynFlags
-- ^ Sets the 'DynFlags' to be appropriate to the optimisation level
updOptLevel n dfs =
    dfs2{ optLevel = final_n }
    where final_n = max 0 (min 2 n) -- Clamp to 0 <= n <= 2
          dfs1 = foldr (flip dopt_unset) dfs  remove_dopts
          dfs2 = foldr (flip dopt_set)   dfs1 extra_dopts
          extra_dopts  = [ f | (ns,f) <- optLevelFlags, final_n `elem` ns ]
          remove_dopts = [ f | (ns,f) <- optLevelFlags, final_n `notElem` ns ]


-- -------------------------------------------------------------------
-- Parsing the dynamic flags

-- | Parse dynamic flags from a list of command line arguments. Return the
-- EITHER parsed 'DynFlags', the left-over arguments, and a list of warnings
-- OR a list of located errors if such occurred during parsing
-- (such as unknown flags or missing arguments).
parseDynamicFlags :: DynFlags -> [Located String]
    -> Either [Located String] (DynFlags, [Located String], [Located String])
                             -- ^ Updated 'DynFlags', left-over arguments, and
                             -- list of warnings.
parseDynamicFlags dflags0 args =
    let ((leftover, errs, warns), dflags1)
            = runCmdLine (processArgs dynamic_flags args) dflags0
    in
    if null errs
       then Right (dflags1, leftover, warns)
       else Left errs


-- -------------------------------------------------------------------
-- DynFlags specifications

allFlags :: [String]
allFlags = map ('-':) $
        [ flagName flag | flag <- dynamic_flags, ok (flagOptKind flag) ] ++
        map ("fno-"++) flags ++
        map ("f"++) flags ++
        map ("f"++) flags'
    where ok (PrefixPred _ _) = False
          ok _ = True
          flags  = [ name | (name, _, _) <- fFlags ]
          flags' = [ name | (name, _, _) <- fLangFlags

-- ---------------------------
-- The main flags themselves
dynamic_flags :: [Flag (CmdLineP DynFlags)]
dynamic_flags =
  [ Flag "n"            (NoArg (setDynFlag Opt_DryRun))
  , Flag "v"            (OptIntSuffix setVerbosity)
  , Flag "ferror-spans" (NoArg (setDynFlag Opt_ErrorSpans))

    ---- Specific phases ----
  , Flag "pgmlo"    (hasArg (\f d -> d{ pgm_lo  = (f,[])}))
  , Flag "pgmlc"    (hasArg (\f d -> d{ pgm_lc  = (f,[])}))
  , Flag "pgma"     (hasArg (\f d -> d{ pgm_a   = (f,[])}))
  , Flag "pgml"     (hasArg (\f d -> d{ pgm_l   = (f,[])}))

  , Flag "optlo"    (hasArg (\f d -> d{ opt_lo  = f : opt_lo d}))
  , Flag "optlc"    (hasArg (\f d -> d{ opt_lc  = f : opt_lc d}))
  , Flag "opta"     (hasArg (\f d -> d{ opt_a   = f : opt_a d}))
  , Flag "optl"     (hasArg addOptl)

    ---- Linking ----
  , Flag "no-link"  (noArg (\d -> d{ gacLink=NoLink }))
  , Flag "shared"   (noArg (\d -> d{ gacLink=LinkDynLib }))

    ---- Libraries ----
  , Flag "L"    (Prefix    addLibraryPath)
  , FLag "l"    (AnySuffix (upd . addOptl))

    ---- Output Redirection ----
  , Flag "odir"         (hasArg setObjectDir)
  , Flag "o"            (SepArg (upd . setOutputFile . Just))
  , Flag "osuf"         (hasArg setObjectSuf)
  , Flag "tmpdir"       (hasArg setTmpDir)
  , Flag "outputdir"    (hasArg setOutputDir)

    ---- Keeping temporary files ----
  , Flag "keep-s-file"      (NoArg (setDynFlag Opt_KeepSFile))
  , Flag "keep-s-files"     (NoArg (setDynFlag Opt_KeepSFile))
  , Flag "keep-llvm-file"   (NoArg (setDynFlag Opt_KeepLlvmFiles))
  , Flag "keep-llvm-files"  (NoArg (setDynFlag Opt_KeepLlvmFiles))
  , Flag "keep-tmp-files"   (NoArg (setDynFlag Opt_KeepTmpFiles))

    ---- Miscellaneous ----
  , Flag "no-auto-link-packages" (NoArg (unSetDynFlag Opt_AutoLinkPackages))

    ---- recompilation checker ----
  , Flag "no-recomp"    (NoArg (do { unSetDynFlag Opt_ForceRecomp
                                   ; deprecate "Use -fno-force-recomp instead" }))
  , Flag "recomp"       (NoArg (do { setDynFlag Opt_ForceRecomp
                                   ; deprecate "Use -fforce-recomp-instead" }))

    ---- Include/Import Paths ----
  , Flag "I"    (Prefix addIncludePath)
  , Flag "i"    (OptPrefix addImportPath)

    ---- Debugging ----
  , Flag "ddump-asm"        (setDumpFlag Opt_D_dump_asm)
  , Flag "ddump-llvm"       (NoArg (do { setObjTarget AlcLlvm
                                       ; setDumpFlag' Opt_D_dump_llvm}))
  , Flag "ddump-parsed"     (setDumpFlag Opt_D_dump_parsed)
  , Flag "ddump-to-file"    (setDumpFlag Opt_DumpToFile)
  , Flag "dshow-passes"     (NoArg (do forceRecompile
                                       setVerbosity (Just 2)))

    ---- Warning opts ----
  , Flag "W"        (NoArg (mapM_ setDynFlag    minusWOpts))
  , Flag "Werror"   (NoArg (setDynFlag          Opt_WarnIsError))
  , Flag "Wwarn"    (NoArg (unSetDynFlag        Opt_WarnIsError))
  , Flag "w"        (NoArg (mapM_ unSetDynFlag minuswRemovesOpts))

    ---- Optimisation flags ----
  , Flag "O"        (noArg (setOptLevel 1))
  , Flag "Onot"     (noArgDF (setOptLevel 0) "Use -O0 instead")
  , Flag "O"        (OptIntSuffix (\mb_n -> upd (setOptLevel (mb_n `orElse` 1))))

    ---- Compiler flags ----
  , Flag "fasm"         (NoArg (setObjTarget AlcAsm))
  , Flag "fllvm"        (NoArg (setObjTarget AlcLlvm))
  , Flag "fno-code"     (NoArg (do upd $ -> d{ gacLink=NoLink }
                                   setTarget AlcNothing))
  ]
  ++ map (mkFlag turnOn  "f"    setDynFlag  ) fFlags
  ++ map (mkFlag turnOff "fno-" unSetDynFlag) fFlags

type TurnOnFlag = Bool  -- True  <=> we are turning the flag on
                        -- False <=> we are turning the flag off
turnOn  :: TurnOnFlag; turnOn = True
turnOff :: TurnOnFlag; turnOff = False

type FlagSpec flag =
    ( String    -- Flag in string form
    , flag      -- Flag in internal form
    , TurnOnFlag -> DynP ())  -- Extra action to run when the flag is found
                              -- Typically, emit a warning or error

mkFlag :: TurnOnFlag        -- ^ True <=> it should be turned on
       -> String            -- ^ The flag prefix
       -> (flag -> DynP ()) -- ^ What to do when the flag is found
       -> FlagSpec flag     -- ^ Specification of this particular flag
       -> Flag (CmdLineP DynFlags)
mkFlag turn_on flagPrefix f (name, flag, extra_action) =
    Flag (flagPrefix ++ name) (NoArg (f flag >> extra_action turn_on))

useInstead :: String -> TurnOnFlag -> DynP ()
useInstead flag turn_on =
    deprecate ("Use -f" ++ no ++ flag ++ " instead")
  where no = if turn_on then "" else "no-"

nop :: TurnOnFlag -> DynP ()
nop _ = return ()

-- | These @-f\<blah>\>@ flags can all be reversed with @-fno-\<blah\>@
fFlags :: [FlagSpec DynFlag]
fFlags = [
  ( "warn-unreachable-code",        Opt_WarnUnreachableCode, nop),
  ( "warn-unused-function",         Opt_WarnUnusedFunction, nop),
  ( "warn-unused-parameter",        Opt_WarnUnusedParameter, nop),
  ( "warn-unused-result",           Opt_WarnUnusedResult, nop),
  ( "warn-unused-variable",         Opt_WarnUnusedVariable, nop),
  ( "warn-uninitialized",           Opt_WarnUnitialized, nop),
  ( "warn-type-limits",             Opt_WarnTypeLimits, nop),
  ( "cse",                          Opt_CSE, nop),
  ( "force-recomp",                 Opt_ForceRecomp, nop),
  ( "regs-graph",                   OptRegsGraph, nop)
  ]
