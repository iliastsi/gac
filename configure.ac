dnl == autoconf source for the Glasgow FP tools ==
dnl (run "grep '^dnl \*' configure.ac | sed -e 's/dnl / /g; s/\*\*/   +/g;'"
dnl  (or some such) to see the outline of this file)
dnl
#
# (c) Tsitsimpis Ilias 2011
#
# Configure script template for GAC
#
# Process with 'autoreconf' to get a working configure script.
#
# For the generated configure script, do "./configure --help" to
# see what flags are available. (Better yet, read the documentation!)
#

AC_INIT([The Glorious Alan Compilation System], [0.4.0], [iliastsi@hotmail.com], [gac])

# Set this to YES for a released version, otherwise NO
: ${RELEASE=YES}


AC_SUBST([CONFIGURE_ARGS], [$ac_configure_args])

dnl ----------------------------------------------------------
dnl ** Find unixy sort and find commands, 
dnl ** which are needed by FP_SETUP_PROJECT_VERSION

dnl ** Find find command (for Win32's benefit)
FP_PROG_FIND

dnl ** Find sort command (for the benefit of Win32 environs)
FP_PROG_SORT

dnl ----------------------------------------------------------
FP_SETUP_PROJECT_VERSION

# First off, a distrib sanity check..
AC_CONFIG_SRCDIR([src/gac.cabal.in])

dnl * We require autoconf version 2.52
AC_PREREQ([2.68])

# -------------------------------------------------------------------------
# Prepare to generate the following header files
#
#
AC_CONFIG_HEADER(src/config.h)

# No, semi-sadly, we don't do `--srcdir'...
if test x"$srcdir" != 'x.' ; then
    echo "This configuration does not support the \`--srcdir' option.."
    exit 1
fi

dnl --------------------------------------------------------------
dnl * Project specific configuration options
dnl --------------------------------------------------------------

dnl ** What command to use to compile compiler sources ?
dnl --------------------------------------------------------------

AC_ARG_WITH([ghc],
[AC_HELP_STRING([--with-ghc=ARG],
  [Use ARG as the path to GHC  [default=autodetect]])],
  [WithGhc="$withval"],
  [if test "$GHC" = ""; then
    AC_PATH_PROG([GHC], [ghc])
  fi
  WithGhc="$GHC"])


dnl ** Tell the make system which OS we are using
dnl $OSTYPE is set by the operating system to "msys" or "cygwin" or something 
AC_SUBST(OSTYPE)

if test "$WithGhc" != ""; then
  FPTOOLS_GHC_VERSION([GhcVersion], [GhcMajVersion], [GhcMinVersion], [GhcPatchLevel])dnl

  if test "$GhcMajVersion" = "unknown" -o "$GhcMinVersion" = "unknown"; then
     AC_MSG_ERROR([Cannot determine the version of $WithGhc.  Is it really GHC?])
  fi

  AC_SUBST(GhcVersion)dnl
  AC_SUBST(GhcMajVersion)dnl
  AC_SUBST(GhcMinVersion)dnl
  AC_SUBST(GhcPatchLevel)dnl
  GhcMinVersion2=`echo "$GhcMinVersion" | sed 's/^\\(.\\)$/0\\1/'`
  GhcCanonVersion="$GhcMajVersion$GhcMinVersion2"
  FP_COMPARE_VERSIONS($GhcVersion,[-ge],[6.10.2],
                      [ghc_ge_6102=YES], [ghc_ge_6102=NO])
  if test $GhcCanonVersion -ge 611; then ghc_ge_611=YES; else ghc_ge_611=NO; fi
  if test $GhcCanonVersion -ge 613; then ghc_ge_613=YES; else ghc_ge_613=NO; fi
  AC_SUBST(ghc_ge_6102)dnl
  AC_SUBST(ghc_ge_611)dnl
  AC_SUBST(ghc_ge_613)dnl
fi

dnl ** Must have GHC to build GAC
if test "$WithGhc" = ""; then
   AC_MSG_ERROR([GHC is required unless bootstrapping from .hc files.])
fi
FP_COMPARE_VERSIONS([$GhcVersion],[-lt],[6.10],
  [AC_MSG_ERROR([GHC version 6.10 or later is required to compile GHC.])])dnl

if test `expr $GhcMinVersion % 2` = "1"; then
   if test "$EnableBootstrapWithDevelSnaphost" = "NO"; then
      AC_MSG_ERROR([
   $WithGhc is a development snapshot of GHC, version $GhcVersion.
   Compiling using this version of GHC is not supported, and may not
   work.  Use --enable-bootstrap-with-devel-snapshot to try it anyway,
   or --with-ghc to specify a different GHC to use.])
   fi
fi;

# GHC is passed to Cabal, so we need a native path
if test "${WithGhc}" != ""
then
    ghc_host=`"${WithGhc}" +RTS --info | grep 'Host platform' | sed -e 's/.*, "//' -e 's/")//'`

    if test "$ghc_host" = "i386-unknown-mingw32"
    then
        if test "${OSTYPE}" = "msys"
        then
            WithGhc=`echo "${WithGhc}" | sed "s#^/\([a-zA-Z]\)/#\1:/#"`
        else
            # Canonicalise to <drive>:/path/to/ghc
            WithGhc=`cygpath -m "${WithGhc}"`
        fi
        echo "GHC path canonicalised to: ${WithGhc}"
    fi
fi
AC_SUBST([WithGhc])

FP_ICONV
FP_GMP

dnl--------------------------------------------------------------------
dnl * Choose host(/target/build) platform
dnl--------------------------------------------------------------------
dnl If we aren't explicitly told what values to use with configure flags,
dnl we ask the ghc compiler what platform it is for

if test "${WithGhc}" != ""
then
        bootstrap_host=`"${WithGhc}" +RTS --info | grep '^ ,("Host platform"' | sed -e 's/.*, "//' -e 's/")//' | tr -d '\r'`
        bootstrap_target=`"${WithGhc}" +RTS --info | grep '^ ,("Target platform"' | sed -e 's/.*, "//' -e 's/")//' | tr -d '\r'`
        if test "$bootstrap_host" != "$bootstrap_target"
        then
                echo "Bootstrapping GHC is a cross compiler. This probably isn't going to work"
        fi
fi

# We have to run these unconditionally, but we may discard their
# results in the following code
AC_CANONICAL_BUILD
AC_CANONICAL_HOST
AC_CANONICAL_TARGET

FPTOOLS_SET_PLATFORM_VARS

exeext=''
soext='.so'
case $host in
*-unknown-cygwin32)
    AC_MSG_WARN([GHC does not support the Cygwin target at the moment])
    AC_MSG_WARN([I'm assuming you wanted to build for i386-unknown-mingw32])
    exit 1
    ;;
*-unknown-mingw32)
    exeext='.exe'
    soext='.dll'
    ;;
i386-apple-darwin|powerpc-apple-darwin)
    soext='.dylib'
    ;;
x86_64-apple-darwin)
    soext='.dylib'
    ;;
esac

checkArch() {
    case $1 in
    alpha|arm|hppa|hppa1_1|i386|ia64|m68k|mips|mipseb|mipsel|powerpc|powerpc64|rs6000|s390|sparc|sparc64|vax|x86_64)
        ;;
    *)
        echo "Unknown arch $1"
        exit 1
        ;;
    esac
}

checkVendor() {
    case $1 in
    dec|unknown|hp|apple|next|sun|sgi|ibm)
        ;;
    *)
        echo "Unknown vendor $1"
        exit 1
        ;;
    esac
}

checkOS() {
    case $1 in
    linux|freebsd|netbsd|openbsd|dragonfly|osf1|osf3|hpux|linuxaout|kfreebsdgnu|freebsd2|solaris2|cygwin32|mingw32|darwin|gnu|nextstep2|nextstep3|sunos4|ultrix|irix|aix|haiku)
        ;;
    *)
        echo "Unknown OS '$1'"
        exit 1
        ;;
    esac
}

BuildPlatform="$BuildArch-$BuildVendor-$BuildOS"
BuildPlatform_CPP=`echo "$BuildPlatform" | sed -e 's/\./_/g' -e 's/-/_/g'`
BuildArch_CPP=`    echo "$BuildArch"     | sed -e 's/\./_/g' -e 's/-/_/g'`
BuildVendor_CPP=`  echo "$BuildVendor"   | sed -e 's/\./_/g' -e 's/-/_/g'`
BuildOS_CPP=`      echo "$BuildOS"       | sed -e 's/\./_/g' -e 's/-/_/g'`

checkArch "$BuildArch"
checkVendor "$BuildVendor"
checkOS "$BuildOS"

HostPlatform="$HostArch-$HostVendor-$HostOS"
HostPlatform_CPP=`echo "$HostPlatform" | sed -e 's/\./_/g' -e 's/-/_/g'`
HostArch_CPP=`    echo "$HostArch"     | sed -e 's/\./_/g' -e 's/-/_/g'`
HostVendor_CPP=`  echo "$HostVendor"   | sed -e 's/\./_/g' -e 's/-/_/g'`
HostOS_CPP=`      echo "$HostOS"       | sed -e 's/\./_/g' -e 's/-/_/g'`

checkArch "$HostArch"
checkVendor "$HostVendor"
checkOS "$HostOS"

TargetPlatform="$TargetArch-$TargetVendor-$TargetOS"
TargetPlatform_CPP=`echo "$TargetPlatform" | sed -e 's/\./_/g' -e 's/-/_/g'`
TargetArch_CPP=`    echo "$TargetArch"     | sed -e 's/\./_/g' -e 's/-/_/g'`
TargetVendor_CPP=`  echo "$TargetVendor"   | sed -e 's/\./_/g' -e 's/-/_/g'`
TargetOS_CPP=`      echo "$TargetOS"       | sed -e 's/\./_/g' -e 's/-/_/g'`

checkArch "$TargetArch"
checkVendor "$TargetVendor"
checkOS "$TargetOS"

# Verify that the installed GHC is capable of generating
# code for the requested build platform.
if test "$build" != "$bootstrap_target"
then
    echo "This GHC (${WithGhc}) does not generate code for the build platform"
    echo "   GHC target platform    : $bootstrap_target"
    echo "   Desired build platform : $BuildPlatform"
    exit 1
fi

AC_SUBST(BuildPlatform)
AC_SUBST(HostPlatform)
AC_SUBST(TargetPlatform)
AC_SUBST(HostPlatform_CPP)
AC_SUBST(BuildPlatform_CPP)
AC_SUBST(TargetPlatform_CPP)

AC_SUBST(HostArch_CPP)
AC_SUBST(BuildArch_CPP)
AC_SUBST(TargetArch_CPP)

AC_SUBST(HostOS_CPP)
AC_SUBST(BuildOS_CPP)
AC_SUBST(TargetOS_CPP)

AC_SUBST(HostVendor_CPP)
AC_SUBST(BuildVendor_CPP)
AC_SUBST(TargetVendor_CPP)

AC_SUBST(exeext)
AC_SUBST(soext)

AC_ARG_WITH(hc,
[AC_HELP_STRING([--with-hc=ARG],
        [Use ARG as the path to the compiler for compiling ordinary
         Haskell code  (default= value of --with-ghc)])],
[WithHc="$withval"],
[WithHc=$WithGhc]
)
AC_SUBST(WithHc)

AC_PATH_PROGS(NHC,nhc nhc98)
AC_PATH_PROG(HBC,hbc)


dnl ** Which ld to use?
dnl --------------------------------------------------------------
FP_ARG_WITH_PATH_GNU_PROG([LD], [ld])
LdCmd="$LD"
AC_SUBST([LdCmd])

dnl ** Which nm to use?
dnl --------------------------------------------------------------
FP_ARG_WITH_PATH_GNU_PROG([NM], [nm])
NmCmd="$NM"
AC_SUBST([NmCmd])


dnl --------------------------------------------------------------
dnl End of configure script option section
dnl --------------------------------------------------------------


dnl --------------------------------------------------------------
dnl * General configuration checks
dnl --------------------------------------------------------------

dnl ** Can the unix package be built?
dnl --------------------------------------------------------------

if test x"$TargetPlatform" = x"i386-unknown-mingw32"; then
   GhcLibsWithUnix=NO
else
   GhcLibsWithUnix=YES
fi
AC_SUBST([GhcLibsWithUnix])

dnl ** does #! work?
AC_SYS_INTERPRETER()

dnl ** check for Python
AC_PATH_PROG(PythonCmd,python)

dnl ** figure out how to invoke cpp directly (gcc -E is no good)
AC_PROG_CPP

dnl ** figure out how to do context diffs
FP_PROG_CONTEXT_DIFF

dnl Let's make sure install-sh is executable here.  If we got it from
dnl a darcs repo, it might not be (see bug #978).
chmod +x install-sh
dnl ** figure out how to do a BSD-ish install
AC_PROG_INSTALL

dnl If you can run configure, you certainly have /bin/sh
AC_DEFINE([HAVE_BIN_SH], [1], [Define to 1 if you have /bin/sh.])

dnl ** how to invoke `ar' and `ranlib'
FP_PROG_AR_SUPPORTS_ATFILE
FP_PROG_AR_NEEDS_RANLIB
FP_PROG_AR_SUPPORTS_INPUT

dnl ** Check to see whether ln -s works
AC_PROG_LN_S


dnl ** Find the path to sed
AC_PATH_PROGS(SedCmd,gsed sed,sed)


dnl ** check for time command
AC_PATH_PROG(TimeCmd,time)

dnl ** check for tar
dnl   if GNU tar is named gtar, look for it first.
AC_PATH_PROGS(TarCmd,gnutar gtar tar,tar)

dnl ** check for patch
dnl if GNU patch is named gpatch, look for it first
AC_PATH_PROGS(PatchCmd,gpatch patch, patch)

dnl ** check for ghc-pkg command
FP_PROG_GHC_PKG

dnl ** check for installed happy binary + version
FPTOOLS_HAPPY

dnl ** check for installed alex binary + version
FPTOOLS_ALEX

dnl --------------------------------------------------
dnl ### program checking section ends here ###
dnl --------------------------------------------------

dnl --------------------------------------------------
dnl * Platform header file and syscall feature tests
dnl ### checking the state of the local header files and syscalls ###

dnl ** check for full ANSI header (.h) files
AC_HEADER_STDC

dnl ** Enable large file support.  NB. do this before testing the type of
dnl    off_t, because it will affect the result of that test.
AC_SYS_LARGEFILE

dnl ** what are the sizes of various types
AC_CHECK_SIZEOF(char,               1)
AC_CHECK_SIZEOF(double,             8)
AC_CHECK_SIZEOF(float,              4)
AC_CHECK_SIZEOF(int,                4)
AC_CHECK_SIZEOF(long,               4)
AC_CHECK_SIZEOF(short,              2)
AC_CHECK_SIZEOF(unsigned char,      1)
AC_CHECK_SIZEOF(unsigned int,       4)
AC_CHECK_SIZEOF(unsigned long,      4)
AC_CHECK_SIZEOF(unsigned short,     2)
AC_CHECK_SIZEOF(void *,             4)


dnl ################################################################
dnl Check for libraries
dnl ################################################################

dnl ** check whether we need -ldl to get dlopen()

AC_CHECK_LIB(dl, dlopen,
    [HaveLibDL=YES
     AC_DEFINE([HAVE_LIBDL], [1], [Define to 1 if you need -ldl to get dlopen().])
     LIBS="$LIBS -ldl"],
    [HaveLibDL=NO])
AC_SUBST(HaveLibDL)

dnl --------------------------------------------------
dnl * Miscellaneous feature tests
dnl --------------------------------------------------

dnl ** are we big endian?
AC_C_BIGENDIAN
FPTOOLS_FLOAT_WORD_ORDER_BIGENDIAN

dnl ** check for leading underscores in symbol names
FP_LEADING_UNDERSCORE

dnl ** check for ld, whether it has an -x option, and if it is GNU ld
FP_PROG_LD_X
FP_PROG_LD_IS_GNU

LIBRARY_VERSION(base)
LIBRARY_VERSION(Cabal)
LIBRARY_gac_VERSION="$ProjectVersion"
AC_SUBST(LIBRARY_gac_VERSION)

if grep '	' src/ghc.cabal.in 2>&1 >/dev/null; then
   AC_MSG_ERROR([src/ghc.cabal.in contains tab characters; please remove them])
fi

AC_CONFIG_FILES([Makefile src/gac.cabal])
AC_OUTPUT

# We get caught by
#     http://savannah.gnu.org/bugs/index.php?1516
#     $(eval ...) inside conditionals causes errors
# with make 3.80, so warn the user if it looks like they're about to
# try to use it.
# We would use "grep -q" here, but Solaris's grep doesn't support it.
checkMake380() {
    if $1 --version 2>&1 | head -1 | grep 'GNU Make 3\.80' > /dev/null
    then
        echo
        echo "WARNING: It looks like \"$1\" is GNU make 3.80."
        echo "This version cannot be used to build GAC."
        echo "Please use GNU make >= 3.81."
    fi
}

checkMake380 make
checkMake380 gmake

echo ["
----------------------------------------------------------------------
Configure completed successfully.

   Building GAC version  : $ProjectVersion

   Build platform        : $BuildPlatform
   Host platform         : $HostPlatform
   Target platform       : $TargetPlatform
"]

echo ["\
   Compiling using   : $WithGhc
      which is version   : $GhcVersion
"]

echo ["\
   ld       : $LdCmd
   Happy    : $HappyCmd ($HappyVersion)
   Alex     : $AlexCmd ($AlexVersion)
   Python   : $PythonCmd"]

echo ["----------------------------------------------------------------------
"]

echo "\
For a standard build of GAC (fully optimised), type (g)make.
"
