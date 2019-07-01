dnl qcd.m4 -- checks for qcd setup (shower, PYTHIA, MPI)
dnl

include('aux.m4')

AC_DEFUN([WO_PROG_QCD],
[dnl
AC_REQUIRE([AC_PROG_FC])

### Choice to enable or disable (internal) PYTHIA6 package

AC_ARG_ENABLE([pythia6],
  [AS_HELP_STRING([--enable-pythia6],
    [enable internal PYTHIA6 for hadronization [[yes]]])],
  [], [enable_pythia6="yes"])

AC_CACHE_CHECK([whether we want to enable PYTHIA6], 
[wo_cv_pythia6],
[dnl
if test "$enable_pythia6" = "yes"; then
  wo_cv_pythia6=yes
else
  wo_cv_pythia6=no
fi])

if test "$enable_pythia6" = "yes"; then
  PYTHIA6_AVAILABLE_FLAG=".true."
  AC_MSG_CHECKING([for PYTHIA6])
  AC_MSG_RESULT([(enabled)])
else
  PYTHIA6_AVAILABLE_FLAG=".false."
  AC_MSG_CHECKING([for PYTHIA6])
  AC_MSG_RESULT([(disabled)])
fi
AC_SUBST(PYTHIA_AVAILABLE_FLAG)

AM_CONDITIONAL([PYTHIA6_AVAILABLE], 
   [test "$PYTHIA6_AVAILABLE_FLAG" = ".true."])
])
