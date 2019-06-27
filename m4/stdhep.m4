dnl stdhep.m4 -- checks for STDHEP library
dnl

include([aux.m4])

### Sets LDFLAGS_STDHEP and the conditional STDHEP_AVAILABLE if successful
### Also: STDHEP_VERSION 
AC_DEFUN([WO_PROG_STDHEP],
[dnl
AC_REQUIRE([AC_PROG_FC])

AC_ARG_ENABLE([stdhep],
  [AS_HELP_STRING([--enable-stdhep],
    [enable STDHEP for binary event files [[yes]]])],
  [], [enable_stdhep="yes"])

if test "$enable_stdhep" = "yes"; then

# Guessing the most likely paths

  wo_stdhep_path="/usr/local/lib:/usr/lib:/opt/local/lib"
  wo_cernlib_path="/usr/local/cern/pro/lib:/cern/pro/lib:/usr/local/lib/cern"

  WO_PATH_LIB(FMCFIO, Fmcfio, libFmcfio.a, $wo_stdhep_path:$wo_cernlib_path)
  WO_PATH_LIB(STDHEP, stdhep, libstdhep.a, $wo_stdhep_path:$wo_cernlib_path)
  if test "$FMCFIO_DIR" = ""; then enable_stdhep="no" 
    if test "$STDHEP_DIR" = ""; then enable_stdhep="no"  
    fi
  fi
  if test "$enable_stdhep" = "yes"; then

  wo_stdhep_libdir="-L$STDHEP_DIR -L$FMCFIO_DIR"
  AC_LANG([Fortran])	
  AC_CHECK_LIB([stdhep -lFmcfio], [stdxwinit],
    [LDFLAGS_STDHEP="$wo_stdhep_libdir -lstdhep -lFmcfio"],
    [enable_stdhep="no"],
    [$wo_stdhep_libdir])

  fi
else
  AC_MSG_CHECKING([for STDHEP])
  AC_MSG_RESULT([(disabled)])
fi
AC_SUBST(LDFLAGS_STDHEP)

if test "$enable_stdhep" = "yes"; then
  STDHEP_AVAILABLE_FLAG=".true."
else
  STDHEP_AVAILABLE_FLAG=".false."
fi
AC_SUBST(STDHEP_AVAILABLE_FLAG)

AM_CONDITIONAL([STDHEP_AVAILABLE], [test "$enable_stdhep" = "yes"])
])
