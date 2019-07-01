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

  WO_PATH_LIB(FMCFIO, Fmcfio, libFmcfio.a, $wo_stdhep_path:$wo_cernlib_path:$FMCFIO_DIR)
  WO_PATH_LIB(STDHEP, stdhep, libstdhep.a, $wo_stdhep_path:$wo_cernlib_path:$STDHEP_DIR)
  if test "$FMCFIO_DIR" = ""; then enable_stdhep="no" 
    if test "$STDHEP_DIR" = ""; then enable_stdhep="no"  
    fi
  fi
  if test "$enable_stdhep" = "yes"; then

  wo_stdhep_libdir="-L$STDHEP_DIR -L$FMCFIO_DIR"
  AC_LANG_PUSH([Fortran])	
  AC_CHECK_LIB([stdhep], [stdxwinit],
    [LDFLAGS_STDHEP="$wo_stdhep_libdir -lstdhep -lFmcfio"],
    [withoutg2c="no"],
    [$wo_stdhep_libdir -lFmcfio])
	
   if test "$withoutg2c" = "no"; then
     AC_LANG([C])	
     AC_CHECK_LIB([g2c], [do_fio],
        [g2cispresent="yes"],
        [g2cispresent="no"])
     AC_LANG([Fortran])	
     if test "$g2cispresent" = "yes"; then
	  AC_MSG_NOTICE([Appending -lg2c to STDHEP LDFLAGS, retrying...])
	  AC_CHECK_LIB([stdhep], [stdxrinit],
	       [LDFLAGS_STDHEP="$wo_stdhep_libdir -lstdhep -lFmcfio -lg2c"],
	       [enable_stdhep="no"],
	       [$wo_stdhep_libdir -lFmcfio -lg2c])
     else
          enable_stdhep="no"
	  AC_MSG_CHECKING([for STDHEP])
	  AC_MSG_RESULT([(disabled)])
     fi
   fi
  AC_LANG_POP() 	
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
