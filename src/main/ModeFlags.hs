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
    PreStartupMode(..), PostStartupMode(..),

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
    | Print String              -- gac --print-foo

showVersionMode, showNumVersionMode, showSupportedExtensionsMode :: Mode
showVersionMode             = mkPreStartupMode ShowVersion
showNumVersionMode          = mkPreStartupMode ShowNumVersion
showSupportedExtensionsMode = mkPreStartupMode ShowSupportedExtensions

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

-- ---------------------------
data PostStartupMode
    = ShowGacUsage                            -- gac -?
    | ShowInfo                                -- gac --info
    | PrintWithDynFlags (DynFlags -> String)  -- gac --print-foo

showGacUsageMode, showInfoMode :: Mode
showGacUsageMode = mkPostStartupMode ShowGacUsage
showInfoMode     = mkPostStartupMode ShowInfo

printWithDynFlagsMode :: (DynFlags -> String) -> Mode
printWithDynFlagsMode f = mkPostStartupMode (PrintWithDynFlags f)

mkPostStartupMode :: PostStartupMode -> Mode
mkPostStartupMode = Right

isShowGacUsageMode :: Mode -> Bool
isShowGacUsageMode (Right ShowGacUsage) = True
isShowGacUsageMode _ = False


-- -------------------------------------------------------------------
-- Parsing the mode flag

parseModeFlags :: [Located String]
               -> Either [Located String]
                    (Maybe Mode,
                     [Located String],
                     [Located String])
parseModeFlags args =
    let ((leftover, errs1, warns), (mModeFlag, errs2, flags')) =
            runCmdLine (processArgs mode_flags args)
                       (Nothing, [], [])
        mode = case mModeFlag of
                    Nothing     -> Nothing
                    Just (m, _) -> Just m
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

showGacUsage :: DynFlags -> IO ()
showGacUsage dflags = do
    let usage_path = gacUsagePath dflags
    usage <- readFile usage_path
    dump usage
    where dump ""          = return ()
          dump ('$':'$':s) = putStr progName >> dump s
          dump (c:s)       = putChar c >> dump s
