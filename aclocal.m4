# Extra autoconf macros for the Glasgow fptools
#
# To be a good autoconf citizen, names of local macros have prefixed with FP_ to
# ensure we don't clash with any pre-supplied autoconf ones.


# FPTOOLS_SET_PLATFORM_VARS
# ----------------------------------
# Set the platform variables
AC_DEFUN([FPTOOLS_SET_PLATFORM_VARS],
[
    ghc_host=`"${WithGhc}" +RTS --info | grep '^ ,("Host platform"' | sed -e 's/.*, "//' -e 's/")//' | tr -d '\r'`
    ghc_target=`"${WithGhc}" +RTS --info | grep '^ ,("Target platform"' | sed -e 's/.*, "//' -e 's/")//' | tr -d '\r'`
    if test "$ghc_host" != "$ghc_target"
    then
            echo "GHC is a cross compiler. This probably isn't going to work"
            exit 1
    fi

    if test "$ghc_target" != ""
    then
        target=$ghc_target
        echo "Target platform inferred as: $target"
    else
        echo "Can't work out target platform"
        exit 1
    fi

    TargetArch=`echo "$target" | sed 's/-.*//'`
    TargetVendor=`echo "$target" | sed -e 's/.*-\(.*\)-.*/\1/'`
    TargetOS=`echo "$target" | sed 's/.*-//'`
])


# FP_COMPARE_VERSIONS(VERSION1, TEST, VERSION2, [ACTION-IF-TRUE], [ACTION-IF-FALSE])
# ----------------------------------------------------------------------------------
# Compare dotted version numbers VERSION1 and VERSION2 lexicographically according
# to TEST (one of -eq, -ne, -lt, -le, -gt, or -ge).
AC_DEFUN([FP_COMPARE_VERSIONS],
[fp_version1=$1; fp_version2=$3
fp_save_IFS=$IFS; IFS='.'
while test x"$fp_version1" != x || test x"$fp_version2" != x
do

  set dummy $fp_version1; shift
  fp_num1=""
  test $[@%:@] = 0 || { fp_num1="[$]1"; shift; }
  test x"$fp_num1" = x && fp_num1="0"
  fp_version1="[$]*"

  set dummy $fp_version2; shift
  fp_num2=""
  test $[@%:@] = 0 || { fp_num2="[$]1"; shift; }
  test x"$fp_num2" = x && fp_num2="0"
  fp_version2="[$]*"

  test "$fp_num1" = "$fp_num2" || break;
done
IFS=$fp_save_IFS
AS_IF([test "$fp_num1" $2 "$fp_num2"], [$4], [$5])[]dnl
])# FP_COMPARE_VERSIONS


dnl
dnl Check for Happy and version.  If we're building GAC, then we need
dnl at least Happy version 1.18.  If there's no installed Happy, we look
dnl for a happy source tree and point the build system at that instead.
dnl
AC_DEFUN([FPTOOLS_HAPPY],
[AC_ARG_WITH([happy],
  [AS_HELP_STRING([--with-happy=ARG],
    [Use ARG as the path to happy [default=autodetect]])],
  [HappyCmd="$withval"],
  [])
AS_IF([test "x$HappyCmd" != "x"],
  [],
  [AC_PATH_PROG(HappyCmd,happy,)])
# Happy is passed to Cabal, so we need a native path

AC_CACHE_CHECK([for version of happy], fptools_cv_happy_version,
changequote(, )dnl
[if test x"$HappyCmd" != x; then
   fptools_cv_happy_version=`"$HappyCmd" -v |
			  grep 'Happy Version' | sed -e 's/Happy Version \([^ ]*\).*/\1/g'` ;
else
   fptools_cv_happy_version="";
fi;
changequote([, ])dnl
])
FP_COMPARE_VERSIONS([$fptools_cv_happy_version],[-lt],[1.18.4],
  [AC_MSG_ERROR([Happy version 1.18.4 or later is required to compile GAC.])])[]
HappyVersion=$fptools_cv_happy_version;
AC_SUBST(HappyVersion)
])

dnl
dnl Check for Alex and version.  If we're building GAC, then we need
dnl at least Alex version 2.3.5.
dnl
AC_DEFUN([FPTOOLS_ALEX],
[AC_ARG_WITH([alex],
  [AS_HELP_STRING([--with-alex=ARG],
    [Use ARG as the path to alex [default=autodetect]])],
  [AlexCmd="$withval"],
  [])
AS_IF([test "x$AlexCmd" != "x"],
  [],
  [AC_PATH_PROG(AlexCmd,alex,)])
# Alex is passed to Cabal, so we need a native path

AC_CACHE_CHECK([for version of alex], fptools_cv_alex_version,
changequote(, )dnl
[if test x"$AlexCmd" != x; then
   fptools_cv_alex_version=`"$AlexCmd" -v |
			  grep 'Alex [Vv]ersion' | sed -e 's/Alex [Vv]ersion \([0-9\.]*\).*/\1/g'` ;
else
   fptools_cv_alex_version="";
fi;
changequote([, ])dnl
])
FP_COMPARE_VERSIONS([$fptools_cv_alex_version],[-lt],[2.3.3],
  [AC_MSG_ERROR([Alex version 2.3.3 or later is required to compile GAC.])])[]
AlexVersion=$fptools_cv_alex_version;
AC_SUBST(AlexVersion)
])


dnl
dnl FPTOOLS_NOCACHE_CHECK prints a message, then sets the
dnl values of the second argument to the result of running
dnl the commands given by the third. It does not cache its
dnl result, so it is suitable for checks which should be
dnl run every time.
dnl
AC_DEFUN([FPTOOLS_NOCACHE_CHECK],
[AC_MSG_CHECKING([$1])
 $3
 AC_MSG_RESULT([$][$2])
])

dnl
dnl FPTOOLS_GHC_VERSION(version)
dnl FPTOOLS_GHC_VERSION(major, minor [, patchlevel])
dnl FPTOOLS_GHC_VERSION(version, major, minor, patchlevel)
dnl
dnl Test for version of installed ghc.  Uses $GHC.
dnl [original version pinched from c2hs]
dnl
AC_DEFUN([FPTOOLS_GHC_VERSION],
[FPTOOLS_NOCACHE_CHECK([version of ghc], [fptools_version_of_ghc],
["${WithGhc-ghc}" --version > conftestghc 2>&1
  cat conftestghc >&AS_MESSAGE_LOG_FD
#Useless Use Of cat award...
  fptools_version_of_ghc=`cat conftestghc | sed -n -e 's/, patchlevel *\([[0-9]]\)/.\1/;s/.* version \([[0-9]][[0-9.]]*\).*/\1/p'`
  rm -fr conftest*
  if test "[$]fptools_version_of_ghc" = ""
  then
    fptools_version_of_ghc='unknown'
  fi
fptools_version_of_ghc[_major]=`echo [$]fptools_version_of_ghc | sed -e 's/^\([[0-9]]\).*/\1/'`
fptools_version_of_ghc[_minor]=`echo [$]fptools_version_of_ghc | sed -e 's/^[[0-9]]\.\([[0-9]]*\).*/\1/'`
fptools_version_of_ghc[_pl]=`echo [$]fptools_version_of_ghc | sed -n -e 's/^[[0-9]]\.[[0-9]]*\.\([[0-9]]*\)/\1/p'`
#
if test "[$]fptools_version_of_ghc[_pl]" = ""
then
  fptools_version_of_ghc[_all]="[$]fptools_version_of_ghc[_major].[$]fptools_version_of_ghc[_minor]"
  fptools_version_of_ghc[_pl]="0"
else
  fptools_version_of_ghc[_all]="[$]fptools_version_of_ghc[_major].[$]fptools_version_of_ghc[_minor].[$]fptools_version_of_ghc[_pl]"
fi
#
ifelse($#, [1], [dnl
[$1]="[$]fptools_version_of_ghc[_all]"
], $#, [2], [dnl
[$1]="[$]fptools_version_of_ghc[_major]"
[$2]="[$]fptools_version_of_ghc[_minor]"
], $#, [3], [dnl
[$1]="[$]fptools_version_of_ghc[_major]"
[$2]="[$]fptools_version_of_ghc[_minor]"
[$3]="[$]fptools_version_of_ghc[_pl]"
], $#, [4], [dnl
[$1]="[$]fptools_version_of_ghc[_all]"
[$2]="[$]fptools_version_of_ghc[_major]"
[$3]="[$]fptools_version_of_ghc[_minor]"
[$4]="[$]fptools_version_of_ghc[_pl]"
])
])
])dnl


# LIBRARY_VERSION(lib)
# --------------------------------
# Gets the version number of a library.
# If $1 is ghc-prim, then we define LIBRARY_ghc_prim_VERSION as 1.2.3
AC_DEFUN([LIBRARY_VERSION],[
LIBRARY_[]translit([$1], [-], [_])[]_VERSION=`grep -i "^version:" libraries/$1/$1.cabal | sed "s/.* //"`
AC_SUBST(LIBRARY_[]translit([$1], [-], [_])[]_VERSION)
])
