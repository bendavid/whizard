dnl pythia8.m4 -- checks for PYTHIA 8 library
dnl

AC_DEFUN([WO_PROG_PYTHON_API],
[dnl
AC_REQUIRE([AX_PYTHON_DEVEL])

AC_ARG_ENABLE([python],
  [AS_HELP_STRING([--enable-python],
    [enable PYTHON/Cython API for WHIZARD [[no]]])],
  [], [enable_python="no"])

if test "$enable_python" = "yes"; then
  AC_MSG_CHECKING([for PYTHON API])
  AC_MSG_RESULT([(enabled)])
else
  AC_MSG_CHECKING([for PYTHON API])
  AC_MSG_RESULT([(disabled)])
fi

if test "$enable_python" = "yes"; then
   PYTHON_API_AVAILABLE_FLAG=".true."
else
   PYTHON_API_AVAILABLE_FLAG=".false."
fi
AC_SUBST([PYTHON_API_AVAILABLE_FLAG])

AM_CONDITIONAL([PYTHON_API_AVAILABLE], [test "$enable_python" = "yes"])
])

