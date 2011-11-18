--------------------------------------------------------------------------------
-- (c) Tsitsimpis Ilias, 2011
--
-- Mode flags
-- (such as help, version, info etc.)
--
--------------------------------------------------------------------------------

module ModeFlags (
    -- * Mode flags and associated configuration types
    Mode(..),
    PreStartupMode(..), PostStartupMode,
    PreLoadMode(..), PostLoadMode(..),

    -- ** Parsing ModeFlags
    parseModeFlags,

    -- ** Manipulating ModeFlags
    showInfo,
    showSupportedExtensions,
    showVersion,
    showGacUsage
  ) where

#include "versions.h"

import CmdLineParser
import DynFlags
import SrcLoc
import Outputable (progName)

import Data.Char
import Data.List


-- -------------------------------------------------------------------
-- The mode flag type

type Mode = Either PreStartupMode PostStartupMode

-- ---------------------------
data PreStartupMode
    = ShowVersion               -- gac -V/--version
    | ShowNumVersion            -- gac --numeric-version
    | ShowSupportedExtensions   -- gac --supported-extensions
    | ShowGacUsage              -- gac -?
    | Print String              -- gac --print-foo

showVersionMode, showNumVersionMode,
    showSupportedExtensionsMode, showGacUsageMode :: Mode
showVersionMode             = mkPreStartupMode ShowVersion
showNumVersionMode          = mkPreStartupMode ShowNumVersion
showSupportedExtensionsMode = mkPreStartupMode ShowSupportedExtensions
showGacUsageMode            = mkPreStartupMode ShowGacUsage

printMode :: String -> Mode
printMode str = mkPreStartupMode (Print str)

mkPreStartupMode :: PreStartupMode -> Mode
mkPreStartupMode = Left

isShowVersionMode :: Mode -> Bool
isShowVersionMode (Left ShowVersion) = True
isShowVersionMode _ = False

isShowNumVersionMode :: Mode -> Bool
isShowNumVersionMode (Left ShowNumVersion) = True
isShowNumVersionMode _ = False

isShowGacUsageMode :: Mode -> Bool
isShowGacUsageMode (Left ShowGacUsage) = True
isShowGacUsageMode _ = False

-- ---------------------------
type PostStartupMode = Either PreLoadMode PostLoadMode

data PreLoadMode
    = ShowInfo                                -- gac --info
    | PrintWithDynFlags (DynFlags -> String)  -- gac --print-foo

showInfoMode :: Mode
showInfoMode     = mkPreLoadMode ShowInfo

printWithDynFlagsMode :: (DynFlags -> String) -> Mode
printWithDynFlagsMode f = mkPreLoadMode (PrintWithDynFlags f)

mkPreLoadMode :: PreLoadMode -> Mode
mkPreLoadMode = Right . Left

data PostLoadMode = PostLoadMode

mkPostLoadMode :: Mode
mkPostLoadMode = Right $ Right PostLoadMode


-- -------------------------------------------------------------------
-- Parsing the mode flag

parseModeFlags :: [Located String]
               -> Either [Located String]
                    (Mode,
                     [Located String],
                     [Located String])
parseModeFlags args =
    let ((leftover, errs1, warns), (mModeFlag, errs2, flags')) =
            runCmdLine (processArgs mode_flags args)
                       (Nothing, [], [])
        mode = case mModeFlag of
                    Nothing     -> mkPostLoadMode
                    Just (m, _) -> m
        errs = errs1 ++ map (mkGeneralLocated "on the commandline") errs2
    in
    if null errs
       then Right (mode, flags' ++ leftover, warns)
       else Left errs


-- -------------------------------------------------------------------
-- The mode flags

type ModeM = CmdLineP (Maybe (Mode, String), [String], [Located String])
    -- mode flags may give rise to new DynFlags
    -- so we collect the new ones and reeturn them.

mode_flags :: [Flag ModeM]
mode_flags =
    [ ---- help / version ----
      Flag "?"                     (PassFlag (setMode showGacUsageMode))
    , Flag "-help"                 (PassFlag (setMode showGacUsageMode))
    , Flag "V"                     (PassFlag (setMode showVersionMode))
    , Flag "-version"              (PassFlag (setMode showVersionMode))
    , Flag "-numeric-version"      (PassFlag (setMode showNumVersionMode))
    , Flag "-info"                 (PassFlag (setMode showInfoMode))
    , Flag "-supported-extensions" (PassFlag (setMode showSupportedExtensionsMode))
    ] ++
    [ Flag k'                      (PassFlag (setMode mode))
    | (k, v) <- compilerInfo,
      let k' = "-print-" ++ map (replaceSpace . toLower) k
          replaceSpace ' ' = '-'
          replaceSpace c   = c
          mode = case v of
                 String str -> printMode str
                 FromDynFlags f -> printWithDynFlagsMode f
    ]

setMode :: Mode -> String -> EwM ModeM ()
setMode newMode newFlag = liftEwM $ do
    (mModeFlag, errs, flags') <- getCmdLineState
    let (modeFlag', errs') =
            case mModeFlag of
                Nothing -> ((newMode, newFlag), errs)
                Just (oldMode, oldFlag) ->
                    case (oldMode, newMode) of
                        -- Normaly, --help/--version/--numeric-version always win
                        _ | isDominantFlag oldMode -> ((oldMode, oldFlag), [])
                          | isDominantFlag newMode -> ((newMode, newFlag), [])
                        -- Otherwise, complain
                        _ -> let err = flagMismatchErr oldFlag newFlag
                             in ((oldMode, oldFlag), err : errs)
    putCmdLineState (Just modeFlag', errs', flags')
  where isDominantFlag f =
            isShowGacUsageMode   f ||
            isShowVersionMode    f ||
            isShowNumVersionMode f

flagMismatchErr :: String -> String -> String
flagMismatchErr oldFlag newFlag =
    "cannot use `" ++ oldFlag ++ "' with `" ++ newFlag ++ "'"


-- -------------------------------------------------------------------
-- Various banners and verbosity output.

-- We print out a Read-friendly string, but a prettier one than the
-- Show instance gives us
showInfo :: DynFlags -> IO ()
showInfo dflags = do
    let sq x = " [" ++ x ++ "\n ]"
    putStrLn $ sq $ concat $ intersperse "\n ," $ map (show . flatten) compilerInfo
  where flatten (k, String v)       = (k, v)
        flatten (k, FromDynFlags f) = (k, f dflags)

showSupportedExtensions :: IO ()
showSupportedExtensions = mapM_ putStrLn supportedExtensions

showVersion :: IO ()
showVersion = putStrLn (PROJECT_NAME ++ ", version " ++ PROJECT_VERSION)

showGacUsage :: IO ()
showGacUsage = do
  putStrLn $ "Usage:"
  putStrLn $ ""
  putStrLn $ "    "++PROG_NAME++" [command-line-options-and-input-files]"
  putStrLn $ ""
  putStrLn $ "To compile and link a complete Alan program, run the compiler like"
  putStrLn $ "so:"
  putStrLn $ ""
  putStrLn $ "    "++PROG_NAME++" Main.alan"
  putStrLn $ ""
  putStrLn $ "where the file Main.alan is located in the current directory."
  putStrLn $ "The linked program will be placed in the file `a.out'."
  putStrLn $ ""
  putStrLn $ "Alternatively, "++PROG_NAME++" can be used to compile files individually.  Each"
  putStrLn $ "input file is guided through (some of the) possible phases of a"
  putStrLn $ "compilation:"
  putStrLn $ ""
  putStrLn $ "    - alc:  run the Haskell compiler proper"
  putStrLn $ "    - as:   run the assembler"
  putStrLn $ "    - ld:   run the linker"
  putStrLn $ ""
  putStrLn $ "For each input file, the phase to START with is determined by the"
  putStrLn $ "file's suffix:"
  putStrLn $ ""
  putStrLn $ "    - .alan     plain Alan                      gac"
  putStrLn $ "    - .s        assembly language               as"
  putStrLn $ "    - other     passed directly to the linker   ld"
  putStrLn $ ""
  putStrLn $ "The phase at which to STOP processing is determined by a command-line"
  putStrLn $ "option:"
  putStrLn $ ""
  putStrLn $ "    -S      stop after generating assembler (.s output)"
  putStrLn $ "    -c      stop after generating object files (.o output)"
  putStrLn $ ""
  putStrLn $ "Other commonly-used options are:"
  putStrLn $ ""
  putStrLn $ "    -v[n]       Control verbosity (n is 0--5, normal verbosity level is 1,"
  putStrLn $ "                  -v alone is equivalent to -v3)"
  putStrLn $ ""
  putStrLn $ "    -O          An `optimising' package of compiler flags, for faster code"
  putStrLn $ ""
  putStrLn $ "Given the above, here are some TYPICAL invocations of "++PROG_NAME++":"
  putStrLn $ ""
  putStrLn $ "    # compile an Alan module to a .o file, optimising:"
  putStrLn $ "    % "++PROG_NAME++" -c -O Foo.alan"
  putStrLn $ "    # link three .o files into an executable called \"test\":"
  putStrLn $ "    % "++PROG_NAME++" -o test Foo.o Bar.o Baz.o"
  putStrLn $ ""
  putStrLn $ "The man page has more information about GAC's *many* options."
  putStrLn $ ""
  putStrLn $ "------------------------------------------------------------------------"
