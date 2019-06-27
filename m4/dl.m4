dnl dl.m4 -- checks for dynamic-linking library
dnl

### Activate dynamic linking: look for 'dlopen'
AC_DEFUN([WO_PROG_DL],
[dnl
AC_ARG_ENABLE([dl],
  [AS_HELP_STRING([--enable-dl],
    [enable libdl for creating/loading process libraries on-the-fly [[yes]]])],
  [], [enable_dl="yes"])

if test "$enable_dl" = "yes"; then
  AC_LANG([C])
  AC_SEARCH_LIBS([dlopen], [dl], [], [enable_dl="no"])
else
  AC_MSG_CHECKING([for dl (dynamic linking)])
  AC_MSG_RESULT([(disabled)])
fi

AM_CONDITIONAL([DL_AVAILABLE], [test "$enable_dl" = "yes"])

### For make check of an installed WHIZARD we have to take
### care of library interdependencies on MAC OS X
case $host in
  *-darwin*)
     DYLD_FLAGS="DYLD_LIBRARY_PATH=\$(pwd)/../src/.libs:\$(pwd)/../src/omega/src/.libs:\${DYLD_LIBRARY_PATH}; export DYLD_LIBRARY_PATH" ;; 
  *)
     DYLD_FLAGS="" ;;
esac
AC_SUBST(DYLD_FLAGS)
])