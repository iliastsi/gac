--------------------------------------------------------------------------------
-- (c) Tsitsimpis Ilias, 2011
--
-- A description of the platform we're compiling for
-- Used by the native code generator
--
--------------------------------------------------------------------------------

module Platform (
    Platform(..),
    Arch(..),
    OS(..),

    defaultTargetPlatform,
    osElfTarget,
    defaultTmpDir
  ) where

#include "gacautoconf.h"


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

-- | This predicates tells us whether the OS supports ELF-like shared libraries
osElfTarget :: OS -> Bool
osElfTarget OSLinux = True
osElfTarget _       = False

-- | This is the target platform as far as the #ifdefs are concerned
-- These are set in includes/gacplatform.h by the autoconf scripts
defaultTargetPlatform :: Platform
defaultTargetPlatform
    = Platform defaultTargetArch defaultTargetOS

-- | Move the evil TARGET_ARCH #ifdefs into Haskell land
defaultTargetArch :: Arch
defaultTargetArch
    | TARGET_ARCH == "i386"     = ArchX86
    | TARGET_ARCH == "x86_64"   = ArchX86_64
    | otherwise                 = error "Platform.buildArch: undefined"

-- | Move the evil TARGET_OS #ifdefs into Haskell land
defaultTargetOS :: OS
defaultTargetOS
    | TARGET_OS == "linux"  = OSLinux
    | otherwise             = OSUnknown

-- | Default TEMPDIR depending on platform
defaultTmpDir :: String
defaultTmpDir
    | TARGET_PLATFORM == "i386-unknown-cygwin32" = /C/TEMP
    | TARGET_PLATFORM == "i386-unknown-mingw32"  = /C/TEMP
    | otherwise                                  = /tmp
