dnl qcd.m4 -- checks for qcd setup (shower, PYTHIA)
dnl

include('aux.m4')

### Sets LDFLAGS_PYTHIA and the conditional PYTHIA_AVAILABLE if successful
### Also: PYTHIA_VERSION 
AC_DEFUN([WO_PROG_QCD],
[dnl
AC_REQUIRE([AC_PROG_FC])

AC_ARG_ENABLE([shower],
  [AS_HELP_STRING([--enable-shower],
    [enable parton showers [[yes]]])],
  [], [enable_shower="yes"])

AC_CACHE_CHECK([whether we want to enable showering], 
[wo_cv_showering],
[dnl
if test "$enable_shower" = "yes"; then
  wo_cv_showering=yes
else
  wo_cv_showering=no
fi])

if test "$enable_shower" = "yes"; then
  PYTHIA_AVAILABLE_FLAG=".true."
  AC_MSG_CHECKING([for PYTHIA])
  AC_MSG_RESULT([(enabled)])
else
  PYTHIA_AVAILABLE_FLAG=".false."
  AC_MSG_CHECKING([for PYTHIA])
  AC_MSG_RESULT([(disabled)])
fi
AC_SUBST(PYTHIA_AVAILABLE_FLAG)

AM_CONDITIONAL([SHOWER_AVAILABLE], 
   [test "$wo_cv_showering" = "yes"])
AM_CONDITIONAL([PYTHIA_AVAILABLE], 
   [test "$PYTHIA_AVAILABLE_FLAG" = ".true."])
])