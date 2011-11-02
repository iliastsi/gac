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
    osElfTarget
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
#if     i386_TARGET_ARCH
defaultTargetArch   = ArchX86
#elif   x86_64_TARGET_ARCH
defaultTargetArch   = ArchX86_64
#else
#error "Platform.buildArch: undefined"
#endif

-- | Move the evil TARGET_OS #ifdefs into Haskell land
defaultTargetOS :: OS
#if     linux_TARGET_OS
defaultTargetOS = OSLinux
#else
defaultTargetOS = OSUnknown
#endif
