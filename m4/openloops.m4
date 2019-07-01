dnl openloops.m4 -- checks for OpenLoops package
dnl


AC_DEFUN([WO_PROG_OPENLOOPS],
[dnl
AC_ARG_ENABLE([openloops],
  [AS_HELP_STRING([--enable-openloops],
     [(experimental) enable OpenLoops for NLO matrix elements [[no]]])],
  [], [enable_openloops="no"])

AC_ARG_WITH([openloops],
  [AS_HELP_STRING([--with-openloops=dir],
     [assume the given directory for OpenLoops])])

unset OPENLOOPS_DIR

if test "$enable_openloops" = "yes"; then

  save_path=$PATH
  save_ld_library_path=$LD_LIBRARY_PATH

  unset OPENLOOPS_DIR
  if test -n "$with_openloops"; then
    WO_PATH_LIB(openloops_lib, [openloops], [libopenloops.${SHRLIB_EXT}], ${with_openloops}/lib)
  else
    WO_PATH_LIB(openloops_lib, [openloops], [libopenloops.${SHRLIB_EXT}], $LD_LIBRARY_PATH)
  fi
  if test "$openloops_lib" != "no"; then
    openloops_libdir=`dirname $openloops_lib`
    OPENLOOPS_DIR=`dirname $openloops_libdir`
  fi

else

  AC_MSG_CHECKING([for OpenLoops])
  AC_MSG_RESULT([(disabled)])

fi

AC_SUBST([OPENLOOPS_DIR])

if test "$enable_openloops" = "yes"; then
   OPENLOOPS_AVAILABLE_FLAG=".true."
else
   OPENLOOPS_AVAILABLE_FLAG=".false."
fi
AC_SUBST([OPENLOOPS_AVAILABLE_FLAG])

AM_CONDITIONAL([OPENLOOPS_AVAILABLE], [test "$enable_openloops" = "yes"])

]) dnl WO_PROG_OPENLOOPS




