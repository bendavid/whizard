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

if test -n "$OPENLOOPS_DIR"; then
  wo_openloops_includes="-I$OPENLOOPS_DIR/lib_src/openloops/mod"
  wo_openloops_ldflags="-L$OPENLOOPS_DIR/lib -lopenloops"
fi

if test "$enable_openloops" = "yes" -a "$openloops_lib" != "no"; then
    AC_MSG_CHECKING([for standard OpenLoops process])
    if test -f "$OPENLOOPS_DIR/proclib/libopenloops_ppll_lt.info"; then
       AC_MSG_RESULT([ OpenLoops process ppll is installed])
       OPENLOOPS_AVAILABLE_FLAG=".true."
    else
       AC_MSG_RESULT([ OpenLoops process ppll is not installed])
       AC_MSG_NOTICE([error: *************************************************************])
       AC_MSG_NOTICE([error: OpenLoops standard process is not installed, please install  ])
       AC_MSG_NOTICE([error:    ppll                                                      ])
       AC_MSG_NOTICE([error: *************************************************************])
       OPENLOOPS_AVAILABLE_FLAG=".false."
       enable_openloops="no"
    fi
    OPENLOOPS_INCLUDES=$wo_openloops_includes
    LDFLAGS_OPENLOOPS=$wo_openloops_ldflags
else
   OPENLOOPS_AVAILABLE_FLAG=".false."
   enable_openloops="no"
fi

AC_SUBST([OPENLOOPS_AVAILABLE_FLAG])
AC_SUBST([OPENLOOPS_INCLUDES])
AC_SUBST([LDFLAGS_OPENLOOPS])

AM_CONDITIONAL([OPENLOOPS_AVAILABLE], [test "$enable_openloops" = "yes"])

]) dnl WO_PROG_OPENLOOPS




