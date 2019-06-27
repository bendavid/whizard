dnl lhapdf.m4 -- checks for LHAPDF library
dnl

### Determine paths to LHAPDF components
### Sets LDFLAGS_LHAPDF and the conditional LHAPDF_AVAILABLE if successful
### Also: LHAPDF_ROOT LHAPDF_VERSION LHAPDF_PDFSETS_PATH
AC_DEFUN([WO_PROG_LHAPDF],
[dnl
AC_REQUIRE([AC_PROG_FC])

AC_ARG_ENABLE([lhapdf],
  [AS_HELP_STRING([--enable-lhapdf],
    [enable LHAPDF for structure functions [[yes]]])],
  [], [enable_lhapdf="yes"])

if test "$enable_lhapdf" = "yes"; then
  if test -n "$LHAPDF_DIR"; then
    wo_lhapdf_config_path=$LHAPDF_DIR/bin:$PATH
  else
    wo_lhapdf_config_path=$PATH
  fi
  AC_PATH_PROG([LHAPDF_CONFIG], [lhapdf-config], [no], 
    [$wo_lhapdf_config_path])

  if test "$LHAPDF_CONFIG" != "no"; then
    LHAPDF_ROOT=`$LHAPDF_CONFIG --prefix`

    AC_MSG_CHECKING([the LHAPDF version])
    LHAPDF_VERSION=`$LHAPDF_CONFIG --version`
    if test -n "$LHAPDF_VERSION"; then
      AC_MSG_RESULT([$LHAPDF_VERSION])
    else
      AC_MSG_RESULT([unknown])
    fi

    AC_MSG_CHECKING([the LHAPDF pdfsets path])
    LHAPDF_PDFSETS_PATH=`$LHAPDF_CONFIG --pdfsets-path`
    if test "$LHAPDF_VERSION" = "5.5.0"; then
      LHAPDF_PDFSETS_PATH=`$LHAPDF_CONFIG --datarootdir`$LHAPDF_PDFSETS_PATH
    fi
    AC_MSG_RESULT([$LHAPDF_PDFSETS_PATH])

  else
    enable_lhapdf="no"
  fi
else
  AC_MSG_CHECKING([for LHAPDF])
  AC_MSG_RESULT([(disabled)])
fi

AC_SUBST(LHAPDF_ROOT)
AC_SUBST(LHAPDF_VERSION)
AC_SUBST(LHAPDF_PDFSETS_PATH)

if test "$enable_lhapdf" = "yes"; then
  wo_lhapdf_libdir="-L$LHAPDF_ROOT/lib"
  AC_LANG([Fortran])
  AC_CHECK_LIB([LHAPDF], [initpdfsetm],
    [LDFLAGS_LHAPDF="$wo_lhapdf_libdir -lLHAPDF"],
    [enable_lhapdf="no"],
    [$wo_lhapdf_libdir])
fi
AC_SUBST(LDFLAGS_LHAPDF)

if test "$enable_lhapdf" = "yes"; then
  LHAPDF_AVAILABLE_FLAG=".true."
else
  LHAPDF_AVAILABLE_FLAG=".false."
fi
AC_SUBST(LHAPDF_AVAILABLE_FLAG)

AM_CONDITIONAL([LHAPDF_AVAILABLE], [test "$enable_lhapdf" = "yes"])
])
