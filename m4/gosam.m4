dnl gosam.m4 -- checks for gosam package and required helper packages
dnl

AC_DEFUN([WO_PROG_GOSAM],
[dnl
AC_ARG_ENABLE([gosam],
  [AS_HELP_STRING([--enable-gosam],
     [(experimental) enable GoSam for NLO matrix elements [[no]]])],
  [], [enable_gosam="no"])

AC_ARG_WITH([gosam],
  [AS_HELP_STRING([--with-gosam=dir],
     [assume the given directory for GoSam])])

unset GOSAM_DIR

if test "$enable_gosam" = "yes"; then

  if test "$with_gosam" = ""; then
    AC_PATH_PROG(gosam_exe, [gosam.py], [no])
  else
    AC_PATH_PROG(gosam_exe, [gosam.py], no, ${with_gosam})
  fi

  if test "$gosam_exe" = "no"; then
    AC_MSG_ERROR([GoSam is enabled but not found])
  else
    GOSAM_DIR=`dirname $gosam_exe`
    echo "Gosam dir is " $GOSAM_DIR
  fi

  AC_MSG_CHECKING([the GoSam version])
  wo_gosam_version=`$gosam_exe --version | $GREP "(rev" | $SED 's/GoSam //g' | $SED 's/ (rev.*$//g'`
  GOSAM_VERSION=$wo_gosam_version
  AC_MSG_RESULT([$wo_gosam_version])
  AC_SUBST([GOSAM_VERSION])

  save_path=$PATH
  save_ld_library_path=$LD_LIBRARY_PATH

  AC_MSG_CHECKING([for gosam_setup_env.sh])
  gosam_env=${GOSAM_DIR}/GoSam/gosam_setup_env.sh
  
  if test -f $gosam_env; then
    AC_MSG_RESULT([$gosam_env])
    . $gosam_env
  else
    AC_MSG_RESULT([no])
    PATH=${GOSAM_DIR}:$PATH
    PKG_CONFIG_PATH=${GOSAM_DIR}/../../lib/GoSam/pkgconfig:${GOSAM_DIR}/../../lib64/GoSam/pkgconfig:$PKG_CONFIG_PATH
  fi

  PATH=$save_path
  LD_LIBRARY_PATH=$save_ld_library_path

else

  AC_MSG_CHECKING([for GoSam])
  AC_MSG_RESULT([(disabled)])

fi

AC_SUBST([GOSAM_DIR])

if test "$enable_gosam" = "yes"; then
   GOSAM_AVAILABLE_FLAG=".true."
else
   GOSAM_AVAILABLE_FLAG=".false."
fi
AC_SUBST([GOSAM_AVAILABLE_FLAG])

AM_CONDITIONAL([GOSAM_AVAILABLE], [test "$enable_gosam" = "yes"])

]) dnl WO_PROG_GOSAM
