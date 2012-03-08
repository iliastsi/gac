--------------------------------------------------------------------------------
-- (c) Tsitsimpis Ilias, 2011-2012
--
-- A description of the platform we're compiling for
-- Used by the native code generator
--
--------------------------------------------------------------------------------

module Platform (
    Platform(..),
    Arch(..),
    OS(..),

    defaultTmpDir
  ) where

#include "config.h"


-- | Contains enough information for the native code generator to emit
-- code for this platform
data Platform
    = Platform
    { platformArch  :: Arch
    , platformOS    :: OS }

-- | Architectures that the native code generator knows about
data Arch
    = ArchX86
    | ArchX86_64
  deriving (Show, Eq)

-- | Operationg systems that the native code generator knows about
-- Having OSUnknown should produce a sensible default, but no promises
data OS
    = OSLinux
    | OSUnknown
  deriving (Show, Eq)

-- | Default TEMPDIR depending on platform
defaultTmpDir :: String
defaultTmpDir
    | HOST_PLATFORM == "i386-unknown-cygwin32" = "/C/TEMP"
    | HOST_PLATFORM == "i386-unknown-mingw32"  = "/C/TEMP"
    | otherwise                                  = "/tmp"
