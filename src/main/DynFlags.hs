--------------------------------------------------------------------------------
-- (c) Tsitsimpis Ilias, 2011-2012
--
-- Dynamic flags
--
-- Most flags are dynamic flags, which means they can change from
-- compilation to compilation. Dynamic Flags can be set only via
-- command-line at the moment.
--
--------------------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
module DynFlags (
    -- * Dynamic flags and associated configuration types
    DynFlag(..),
    ExtensionFlag(..),
    dopt,
    dopt_set,
    dopt_unset,
    xopt,
    xopt_set,
    xopt_unset,
    DynFlags(..),
    GacTarget(..), isObjectTarget, defaultObjectTarget,
    GacLink(..), isNoLink,
    Option(..), showOpt,
    fFlags, xFlags,

    -- ** Manipulating DynFlags
    defaultDynFlags,        -- DynFlags
    initDynFlags,           -- DynFlags -> IO DynFlags

    getOpts,                -- DynFlags -> (DynFlags -> [a]) -> [a]
    getVerbFlag,
    updOptLevel,

    -- ** Parsing DynFlags
    parseDynamicFlags,
    allFlags,

    supportedExtensions,

    -- ** Compiler configuration suitable for display to the user
    Printable(..),
    compilerInfo
  ) where

#include "config.h"

import CmdLineParser
import {-# SOURCE #-} Outputable
import Util
import Maybes       (orElse)
import SrcLoc
import {-# SOURCE #-} ErrUtils

import Data.IORef
import Data.List
import System.IO


-- -------------------------------------------------------------------
-- DynFlags

-- | Enumerates the simple on-of-off dynamic flags
data DynFlag
    -- debugging flags
    = Opt_D_dump_parsed
    | Opt_DumpToFile        -- ^ Append dump output to files instead of stdout

    | Opt_WarnIsError       -- -Werror; makes warnings fatal
    | Opt_WarnUnreachableCode
    | Opt_WarnUnusedFunction
    | Opt_WarnUnusedResult
    | Opt_WarnUnusedVariable
    | Opt_WarnTypeOverflows
    | Opt_WarnWarningsDeprecations
    | Opt_WarnDeprecatedFlags

    -- misc opts
    | Opt_ErrorSpans        -- Include full span info in error messages

    -- keeping stuff
    | Opt_KeepSFiles
    | Opt_KeepObjFiles
    | Opt_KeepLlvmFiles
  deriving (Eq, Show)

data ExtensionFlag
    = Opt_ForwardDecls
    | Opt_MultiDimArrays
    | Opt_ExplicitMain
  deriving (Eq, Show)

-- | Contains not only a collection of 'DynFlag's but also a plethora of
-- information relating to the compilation of a single file or GHC session
data DynFlags = DynFlags {
    gacLink             :: GacLink,
    gacTarget           :: GacTarget,
    verbosity           :: Int,         -- ^ Verbosity level: see Note [Verbosity levels]
    optLevel            :: Int,         -- ^ Optimisation level (0-3)

    -- paths etc
    outputDir           :: Maybe String,
    outputFile          :: Maybe String,

    libraryPaths        :: [String],

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

    -- Package flags
    topDir              :: FilePath,    -- filled in by SysTools

    -- Temporary files
    filesToClean        :: IORef [FilePath],

    -- gac dynamic flags
    flags               :: [DynFlag],
    -- Don't change this without updating extensionFlags:
    extensions          :: [OnOff ExtensionFlag],
    -- extensionFlags should always be equal to
    --      flattenExtensionFlags extensions
    extensionFlags      :: [ExtensionFlag],

    -- | Message output action: use "ErrUtils" instead of this if you can
    log_action          :: Severity -> SrcSpan -> String -> IO ()
  }

-- | The target code type of the compilation (if any)
--
-- Whenever you change the target, also make sure to set 'gacLink' to
-- something sensible.
--
-- 'GacNothing' can be used to avoid generating any output
--
data GacTarget
    = GacLlvm       -- ^ Generate assembly using the llvm code generator
--    | GacAsm        -- ^ Generate assembly using the native code generator
    | GacNothing    -- ^ Don't generate any code
  deriving (Eq, Show)

-- | Will this target result in an object file on the disk?
isObjectTarget :: GacTarget -> Bool
isObjectTarget GacLlvm  = True
isObjectTarget _        = False

-- | What to do in the link step, if there is one
data GacLink
    = NoLink        -- ^ Don't link at all
    | LinkBinary    -- ^ Link object code into a binary
  deriving (Eq, Show)

isNoLink :: GacLink -> Bool
isNoLink NoLink = True
isNoLink _      = False

defaultGacTarget :: GacTarget
defaultGacTarget = defaultObjectTarget

-- | The 'GacTarget' value corresponding to the default way to create
-- object files on the current platform
defaultObjectTarget :: GacTarget
defaultObjectTarget = GacLlvm

-- | Partially initialize a new 'DynFlags' value
initDynFlags :: DynFlags -> IO DynFlags
initDynFlags dflags = do
    refFilesToClean <- newIORef []
    return dflags{
        filesToClean    = refFilesToClean
    }

-- | The normal 'DynFlags'. Note that they is not suitable for use in this form
defaultDynFlags :: DynFlags
defaultDynFlags =
    DynFlags {
        gacLink             = LinkBinary,
        gacTarget           = defaultGacTarget,
        verbosity           = 0,
        optLevel            = 1,

        outputDir           = Nothing,
        outputFile          = Nothing,

        libraryPaths        = [],

        opt_a               = [],
        opt_l               = [],
        opt_lo              = [],
        opt_lc              = [],

        -- initSysTools fills all these in
        topDir              = panic "defaultDynFlags: No topDir",
        pgm_a               = panic "defaultDynFlags: No pgm_a",
        pgm_l               = panic "defaultDynFlags: No pgm_l",
        pgm_lo              = panic "defaultDynFlags: No pgm_lo",
        pgm_lc              = panic "defaultDynFlags: No pgm_lc",
        -- end of initSynTools values

        filesToClean        = panic "defaultDynFlags: No filesToClean",

        flags               = defaultFlags,
        extensions          = [],
        extensionFlags      = flattenExtensionFlags [],

        log_action = \severity _srcSpan msg ->
                        case severity of
                             SevOutput -> printOutput [msg]
                             SevInfo  -> printErrs [msg]
                             SevFatal -> printErrs [msg]
                             _        -> do
                                 hPutChar stderr '\n'
                                 printErrs [msg]
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

-- OnOffs accumulate in reverse order, so we use folr in order to
-- process them in the right order
flattenExtensionFlags :: [OnOff ExtensionFlag] -> [ExtensionFlag]
flattenExtensionFlags = foldr f defaultExtensionFlags
    where f (On f)  flags = f : delete f flags
          f (Off f) flags =     delete f flags
          defaultExtensionFlags = []

-- | Test whether a 'DynFlag' is set
dopt :: DynFlag -> DynFlags -> Bool
dopt f dflags = f `elem` (flags dflags)

-- | Set a 'DynFlag'
dopt_set :: DynFlags -> DynFlag -> DynFlags
dopt_set dfs f = dfs { flags = f : flags dfs }

-- | Unset a 'DynFlag'
dopt_unset :: DynFlags -> DynFlag -> DynFlags
dopt_unset dfs f = dfs{ flags = filter (/= f) (flags dfs) }

-- | Test whether a 'ExtensionFlag' is set
xopt :: ExtensionFlag -> DynFlags -> Bool
xopt f dflags = f `elem` extensionFlags dflags

-- | Set a 'ExtensionFlag'
xopt_set :: DynFlags -> ExtensionFlag -> DynFlags
xopt_set dfs f =
    let onoffs = On f : extensions dfs
    in dfs { extensions = onoffs,
             extensionFlags = flattenExtensionFlags onoffs }

-- | Unset a 'ExtensionFlag'
xopt_unset :: DynFlags -> ExtensionFlag -> DynFlags
xopt_unset dfs f =
    let onoffs = Off f : extensions dfs
    in dfs { extensions = onoffs,
             extensionFlags = flattenExtensionFlags onoffs }

-- | Retrive the options corresponding to a particular @opt_*@ filed in the correct order
getOpts :: DynFlags             -- ^ 'DynFlags' to retrieve the options from
        -> (DynFlags -> [a])    -- ^ Relevant record accessor: one of the @opt_*@ accessors
        -> [a]                  -- ^ Correctly ordered extracted options
getOpts dflags opts = reverse (opts dflags)
        -- We add to the options from the front, so we need to reverse the list

-- | Get the verbosity flag for the current verbosity level. This is fed to
-- other tools, so GAC-specific verbosity flags like @-ddump-most@ are not included
getVerbFlag :: DynFlags -> String
getVerbFlag dflags
    | verbosity dflags >= 3 = "-v"
    | otherwise = ""

setOutputDir, addOptl
    :: String -> DynFlags -> DynFlags
setOutputFile
    :: Maybe String -> DynFlags -> DynFlags
setOutputDir        f d = d{ outputDir = Just f }
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
    dfs{ optLevel = final_n }
    where final_n = max 0 (min 3 n) -- Clamp to 0 <= n <= 3


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
        map ("X"++) supportedExtensions
    where ok (PrefixPred _ _) = False
          ok _ = True
          flags  = [ name | (name, _, _) <- fFlags ]

-- ---------------------------
-- The main flags themselves
dynamic_flags :: [Flag (CmdLineP DynFlags)]
dynamic_flags =
  [ Flag "v"            (OptIntSuffix setVerbosity)
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

    ---- Libraries ----
  , Flag "L"    (Prefix    addLibraryPath)
  , Flag "l"    (AnySuffix (upd . addOptl))

    ---- Output Redirection ----
  , Flag "o"            (SepArg (upd . setOutputFile . Just))
  , Flag "outputdir"    (hasArg setOutputDir)

    ---- Keeping temporary files ----
  , Flag "keep-s-file"      (NoArg (setDynFlag Opt_KeepSFiles))
  , Flag "keep-s-files"     (NoArg (setDynFlag Opt_KeepSFiles))
  , Flag "keep-llvm-file"   (NoArg (setDynFlag Opt_KeepLlvmFiles))
  , Flag "keep-llvm-files"  (NoArg (setDynFlag Opt_KeepLlvmFiles))
  , Flag "keep-obj-file"    (NoArg (setDynFlag Opt_KeepObjFiles))
  , Flag "keep-obj-files"   (NoArg (setDynFlag Opt_KeepObjFiles))

    ---- Debugging ----
  , Flag "ddump-parsed"     (setDumpFlag Opt_D_dump_parsed)
  , Flag "ddump-to-file"    (setDumpFlag Opt_DumpToFile)

    ---- Warning opts ----
  , Flag "W"        (NoArg (mapM_ setDynFlag    minusWOpts))
  , Flag "Werror"   (NoArg (setDynFlag          Opt_WarnIsError))
  , Flag "Wwarn"    (NoArg (unSetDynFlag        Opt_WarnIsError))
  , Flag "Wall"     (NoArg (mapM_ setDynFlag    minusWallOpts))
  , Flag "Wnot"     (NoArg (do { mapM_ unSetDynFlag minusWallOpts
                               ; deprecate "Use -w instead" }))
  , Flag "w"        (NoArg (mapM_ unSetDynFlag minuswRemovesOpts))

    ---- Optimisation flags ----
  , Flag "O"        (noArg (setOptLevel 1))
  , Flag "Onot"     (noArgDF (setOptLevel 0) "Use -O0 instead")
  , Flag "O"        (OptIntSuffix (\mb_n -> upd (setOptLevel (mb_n `orElse` 1))))

    ---- Compiler flags ----
  , Flag "fllvm"        (NoArg (setObjTarget GacLlvm))
  , Flag "fno-code"     (NoArg (do upd $ \d -> d{ gacLink=NoLink }
                                   setTarget GacNothing))
  ]
  ++ map (mkFlag turnOn  "f"    setDynFlag  ) fFlags
  ++ map (mkFlag turnOff "fno-" unSetDynFlag) fFlags
  ++ map (mkFlag turnOn  "X"    setExtensionFlag  ) xFlags
  ++ map (mkFlag turnOff "XNo"  unSetExtensionFlag) xFlags

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

deprecatedForExtension :: String -> TurnOnFlag -> DynP ()
deprecatedForExtension lang turn_on =
    deprecate ("use -X" ++ flag ++ " instead")
  where
    flag | turn_on   = lang
         | otherwise = "No"++lang

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
  ( "warn-unused-result",           Opt_WarnUnusedResult, nop),
  ( "warn-unused-variable",         Opt_WarnUnusedVariable, nop),
  ( "warn-type-overflows",          Opt_WarnTypeOverflows, nop),
  ( "warn-warnings-deprecations",   Opt_WarnWarningsDeprecations, nop),
  ( "warn-deprecations",            Opt_WarnWarningsDeprecations, nop),
  ( "warn_deprecated-flags",        Opt_WarnDeprecatedFlags, nop),
  ( "warn-deprecated-flags",        Opt_WarnDeprecatedFlags, nop)
  ]

supportedExtensions :: [String]
supportedExtensions = [ name' | (name, _, _) <- xFlags, name' <- [name, "No"++name] ]

-- | These -X<blah> flags can all be reversed with -XNo<blah>
xFlags :: [FlagSpec ExtensionFlag]
xFlags = [
  ( "ForwardDeclarations",      Opt_ForwardDecls, nop),
  ( "MultiDimArrays",           Opt_MultiDimArrays, nop),
  ( "ExplicitMain",             Opt_ExplicitMain, nop)
  ]

defaultFlags :: [DynFlag]
defaultFlags =
    standarWarnings

impliedFlags :: [(ExtensionFlag, TurnOnFlag, ExtensionFlag)]
impliedFlags = []

standarWarnings :: [DynFlag]
standarWarnings =
    [ Opt_WarnWarningsDeprecations
    , Opt_WarnDeprecatedFlags
    ]

minusWOpts :: [DynFlag]
minusWOpts =
    standarWarnings ++
    [ Opt_WarnUnreachableCode
    , Opt_WarnUnusedFunction
    , Opt_WarnUnusedVariable
    ]

minusWallOpts :: [DynFlag]
minusWallOpts = minusWOpts ++
    [ Opt_WarnUnusedResult
    , Opt_WarnTypeOverflows
    ]

-- minuswRemovesOpts should be every warning option
minuswRemovesOpts :: [DynFlag]
minuswRemovesOpts = minusWallOpts


-- -------------------------------------------------------------------
-- DynFlags constructors

type DynP = EwM (CmdLineP DynFlags)

upd :: (DynFlags -> DynFlags) -> DynP ()
upd f = liftEwM (do { dfs <- getCmdLineState
                    ; putCmdLineState $! (f dfs) })

-- ---------------------------
-- Constructor functions for OptKind
noArg :: (DynFlags -> DynFlags) -> OptKind (CmdLineP DynFlags)
noArg fn = NoArg (upd fn)

noArgDF :: (DynFlags -> DynFlags) -> String -> OptKind (CmdLineP DynFlags)
noArgDF fn deprec = NoArg (upd fn >> deprecate deprec)

hasArg :: (String -> DynFlags -> DynFlags) -> OptKind (CmdLineP DynFlags)
hasArg fn = HasArg (upd . fn)

hasArgDF :: (String -> DynFlags -> DynFlags) -> String -> OptKind (CmdLineP DynFlags)
hasArgDF fn deprec = HasArg (\s -> do { upd (fn s)
                                      ; deprecate deprec })

intSuffix :: (Int -> DynFlags -> DynFlags) -> OptKind (CmdLineP DynFlags)
intSuffix fn = IntSuffix (\n -> upd (fn n))

setDumpFlag :: DynFlag -> OptKind (CmdLineP DynFlags)
setDumpFlag dump_flag = NoArg (setDumpFlag' dump_flag)

-- ---------------------------
setDynFlag, unSetDynFlag :: DynFlag -> DynP ()
setDynFlag   f = upd (\dfs -> dopt_set dfs f)
unSetDynFlag f = upd (\dfs -> dopt_unset dfs f)

-- ---------------------------
setExtensionFlag, unSetExtensionFlag :: ExtensionFlag -> DynP ()
setExtensionFlag f = do
    upd (\dfs -> xopt_set dfs f)
    sequence_ deps
  where
    deps = [ if turn_on then setExtensionFlag   d
                        else unSetExtensionFlag d
           | (f', turn_on, d) <- impliedFlags, f' == f ]
        -- When you set f, set the ones it implies
        -- NB: use setExtensionFlag recursively, in case the implied flags
        --     implies further flags

unSetExtensionFlag f = upd (\dfs -> xopt_unset dfs f)
    -- When you un-set f, however, we  don't un-set the things it implies

-- ---------------------------
setDumpFlag' :: DynFlag -> DynP ()
setDumpFlag' dump_flag = do
    setDynFlag dump_flag

setVerbosity :: Maybe Int -> DynP ()
setVerbosity mb_n = upd (\dfs -> dfs{ verbosity = mb_n `orElse` 3 })

-- if we're linking a binary, then only targets that produce object
-- code are allowed (requests for other target types are ignored).
setTarget :: GacTarget -> DynP ()
setTarget l = upd set
  where
    set dfs
      | gacLink dfs /= LinkBinary || isObjectTarget l = dfs{ gacTarget = l }
      | otherwise = dfs

-- Changes the target only if we're compiling object code. This is
-- used by -fasm and -fllvm, which switch from one to the other, but
-- not from bytecode to object-code
setObjTarget :: GacTarget -> DynP ()
setObjTarget l = upd set
  where
    set dfs
      | isObjectTarget (gacTarget dfs) = dfs { gacTarget = l }
      | otherwise = dfs

setOptLevel :: Int -> DynFlags -> DynFlags
setOptLevel n dflags =
    updOptLevel n dflags


-- -------------------------------------------------------------------
-- Paths & Libraries

addLibraryPath :: FilePath -> DynP ()
addLibraryPath p =
    upd (\s -> s{libraryPaths = libraryPaths s ++ splitPathList p})

#ifndef mingw32_TARGET_OS
split_marker :: Char
split_marker = ':'  -- not configurable (ToDo)
#endif

splitPathList :: String -> [String]
splitPathList s = filter notNull (splitUp s)
                -- empty paths are ignored: there might be a trailing
                -- ':' in the initial list, for example. Empty paths can
                -- cause confusion when they are translated into -I options
                -- for passing to gcc.
  where
#ifndef mingw32_TARGET_OS
    splitUp xs = split split_marker xs
#else
     -- Windows: 'hybrid' support for DOS-style paths in directory lists.
     --
     -- That is, if "foo:bar:baz" is used, this interpreted as
     -- consisting of three entries, 'foo', 'bar', 'baz'.
     -- However, with "c:/foo:c:\\foo;x:/bar", this is interpreted
     -- as 3 elts, "c:/foo", "c:\\foo", "x:/bar"
     --
     -- Notice that no attempt is made to fully replace the 'standard'
     -- split marker ':' with the Windows / DOS one, ';'. The reason being
     -- that this will cause too much breakage for users & ':' will
     -- work fine even with DOS paths, if you're not insisting on being silly.
     -- So, use either.
    splitUp [] = []
    splitUp (x:':':div:xs) | div `elem` dir_markers
                           = ((x:':':div:p): splitUp rs)
                           where
                              (p,rs) = findNextPath xs
          -- we used to check for existence of the path here, but that
          -- required the IO monad to be threaded through the command-line
          -- parser which is quite inconvenient. The
    splitUp xs = cons p (splitUp rs)
               where
                  (p,rs) = findNextPath xs
                  cons "" xs = xs
                  cons x  xs = x:xs
    -- will be called either when we've consumed nought or the
    -- "<Drive>:/" part of a DOS path, so splitting is just a Q of
    -- finding the next split marker.
    findNextPath xs =
        case break (`elem` split_markers) xs of
             (p, _:ds) -> (p, ds)
             (p, xs)   -> (p, xs)
    split_markers :: [Char]
    split_markers = [':', ';']
    dir_markers :: [Char]
    dir_markers = ['/', '\\']
#endif


-- -------------------------------------------------------------------
-- Compiler Info

data Printable
    = String String
    | FromDynFlags (DynFlags -> String)

compilerInfo :: [(String, Printable)]
compilerInfo =
    [("Project name",               String PROJECT_NAME)
    ,("assembler command",          FromDynFlags $ fst . pgm_a)
    ,("linker command",             FromDynFlags $ fst . pgm_l)
    ,("llvm compiler",              FromDynFlags $ fst . pgm_lc)
    ,("llvm optimiser",             FromDynFlags $ fst . pgm_lo)
    ,("Project version",            String PROJECT_VERSION)
    ,("Host platform",              String HOST_PLATFORM)
    ,("Have llvm code generator",   String "YES")
    ,("Have native code generator", String "NO")
    ,("Have UTF-8 support",         String (if ALEX_VERSION>="3" then "YES" else "NO"))
    ,("LibDir",                     FromDynFlags topDir)
    ]
