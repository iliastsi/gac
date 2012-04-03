dnl == autoconf source for the Alan PL tools ==
dnl (run "grep '^dnl \*' configure.ac | sed -e 's/dnl / /g; s/\*\*/   +/g;'"
dnl  (or some such) to see the outline of this file)
dnl
#
# (c) Tsitsimpis Ilias 2011-2012
#
# Configure script template for GAC
#
# Process with 'autoreconf' to get a working configure script.
#
# For the generated configure script, do "./configure --help" to
# see what flags are available. (Better yet, read the documentation!)
#

AC_INIT([The Glorious Alan Compilation System], [1.0], [iliastsi@hotmail.com], [gac])

AC_SUBST([CONFIGURE_ARGS], [$ac_configure_args])

dnl ----------------------------------------------------------
dnl ** Find unixy sort and find commands,
dnl ** which are needed by FP_SETUP_PROJECT_VERSION
# First off, a distrib sanity check..
AC_CONFIG_SRCDIR([src/gac.cabal.in])

dnl * We require autoconf version 2.52
AC_PREREQ([2.68])

# No, semi-sadly, we don't do `--srcdir'...
if test x"$srcdir" != 'x.' ; then
    echo "This configuration does not support the \`--srcdir' option.."
    exit 1
fi

dnl --------------------------------------------------------------
dnl * Project specific configuration options
dnl --------------------------------------------------------------

dnl ** Tell the make system which OS we are using
dnl $OSTYPE is set by the operating system to "msys" or "cygwin" or something
AC_SUBST(OSTYPE)

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

dnl ** Must have GHC to build gac
if test "$WithGhc" = ""; then
   AC_MSG_ERROR([GHC is required to build gac.])
fi

FPTOOLS_GHC_VERSION([GhcVersion], [GhcMajVersion], [GhcMinVersion], [GhcPatchLevel])dnl
if test "$GhcMajVersion" = "unknown" -o "$GhcMinVersion" = "unknown"; then
 AC_MSG_ERROR([Cannot determine the version of $WithGhc.  Is it really GHC?])
fi
AC_SUBST(GhcVersion)dnl
AC_SUBST(GhcMajVersion)dnl
AC_SUBST(GhcMinVersion)dnl
AC_SUBST(GhcPatchLevel)dnl

FP_COMPARE_VERSIONS([$GhcVersion],[-lt],[7.0.3],
  [AC_MSG_ERROR([GHC version 7.0.3 or later is required to compile gac.])])dnl

if test `expr $GhcMinVersion % 2` = "1"; then
      AC_MSG_ERROR([
   $WithGhc is a development snapshot of GHC, version $GhcVersion.
   Compiling using this version of GHC is not supported, and may not
   work.  Use --with-ghc to specify a different GHC to use.])
fi;

# GHC is passed to Cabal, so we need a native path
ghc_host=`"${WithGhc}" +RTS --info | grep 'Host platform' | sed -e 's/.*, "//' -e 's/")//'`

AC_SUBST([WithGhc])

dnl--------------------------------------------------------------------
dnl * Choose host(/target/build) platform
dnl--------------------------------------------------------------------
dnl We ask the ghc compiler what platform it is for
FPTOOLS_SET_PLATFORM_VARS

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

TargetPlatform="$TargetArch-$TargetVendor-$TargetOS"

checkArch "$TargetArch"
checkVendor "$TargetVendor"
checkOS "$TargetOS"

AC_SUBST(TargetPlatform)
AC_SUBST(TargetArch)
AC_SUBST(TargetOS)
AC_SUBST(TargetVendor)

dnl --------------------------------------------------------------
dnl End of configure script option section
dnl --------------------------------------------------------------

dnl --------------------------------------------------------------
dnl * General configuration checks
dnl --------------------------------------------------------------
dnl ** does #! work?
AC_SYS_INTERPRETER()

dnl ** check for Python
AC_PATH_PROG(PythonCmd,python)
if test "$PythonCmd" = ""; then
    echo "Python is required to run testsuite."
fi

dnl Let's make sure install-sh is executable here.  If we got it from
dnl a repo, it might not be (see bug #978).
chmod +x install-sh
dnl ** figure out how to do a BSD-ish install
AC_PROG_INSTALL

dnl ** Find the path to gcc
AC_PROG_CC(gcc)

dnl ** Find the path to ar
AC_PATH_PROGS(ArCmd,ar,ar)

dnl ** Find the path to llc
AC_PATH_PROGS(LlcCmd,llc,llc)

dnl ** Find the path to opt
AC_PATH_PROGS(OptCmd,opt,opt)

dnl ** Find the path to sed
AC_PATH_PROGS(SedCmd,gsed sed,sed)

dnl ** check for tar
dnl   if GNU tar is named gtar, look for it first.
AC_PATH_PROGS(TarCmd,gnutar gtar tar,tar)

dnl ** Find the path to gzip
AC_PATH_PROGS(GZipCmd,gzip,gzip)

dnl ** check for installed happy binary + version
FPTOOLS_HAPPY

dnl ** set ghc flags
AC_ARG_VAR([HFLAGS], [GHC compiler flags])
if test x"$ac_cv_env_HFLAGS_set" != x
then
        HFLAGS=$ac_cv_env_HFLAGS_value
else
        HFLAGS="-Wall -O2 -dno-debug-output"
fi
AC_SUBST([HFLAGS])

dnl ** set happy flags
AC_ARG_VAR([YFLAGS], [Happy metacompiler flags])
if test x"$ac_cv_env_YFLAGS_set" != x
then
        YFLAGS=$ac_cv_env_YFLAGS_value
else
        YFLAGS="--info --array --ghc --coerce"
fi
AC_SUBST([YFLAGS])

dnl ** check for installed alex binary + version
FPTOOLS_ALEX

dnl ** set alex flags
AC_ARG_VAR([LFLAGS], [Alex metacompiler flags])
if test x"$ac_cv_env_LFLAGS_set" != x
then
        LFLAGS=$ac_cv_env_LFLAGS_value
else
        LFLAGS="--ghc"
fi
AC_SUBST([LFLAGS])

dnl --------------------------------------------------
dnl ### program checking section ends here ###
dnl --------------------------------------------------

if grep '	' src/gac.cabal.in 2>&1 >/dev/null; then
   AC_MSG_ERROR([src/gac.cabal.in contains tab characters; please remove them])
fi

AC_CONFIG_FILES([Makefile src/gac.cabal src/Makefile src/config.h libraries/Makefile])
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
        echo "This version cannot be used to build gac."
        echo "Please use GNU make >= 3.81."
    fi
}

checkMake380 make
checkMake380 gmake

echo ["
----------------------------------------------------------------------
Configure completed successfully.

   Building GAC version  : $PACKAGE_VERSION

   Target platform       : $TargetPlatform
"]

echo ["\
   Compiling using   : $WithGhc
      which is version   : $GhcVersion
"]

echo ["\
   Happy    : $HappyCmd ($HappyVersion)
   Alex     : $AlexCmd ($AlexVersion)
   Python   : $PythonCmd"]

echo ["----------------------------------------------------------------------
"]

echo "\
For a standard build of GAC (fully optimised), type (g)make.
"
