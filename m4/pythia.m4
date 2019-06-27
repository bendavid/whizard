dnl pythia.m4 -- checks for pythia library
dnl

include('aux.m4')

### Sets LDFLAGS_PYTHIA and the conditional PYTHIA_AVAILABLE if successful
### Also: PYTHIA_VERSION 
AC_DEFUN([WO_PROG_PYTHIA],
[dnl
AC_REQUIRE([AC_PROG_FC])

AC_ARG_ENABLE([pythia],
  [AS_HELP_STRING([--enable-pythia],
    [enable PYTHIA for matching w/ parton showers (if not found set PYTHIA_DIR)[[no]]])],
  [], [enable_pythia="no"])

if test "$enable_pythia" = "yes"; then
  PYTHIA_AVAILABLE_FLAG=".true."
  AC_MSG_CHECKING([for PYTHIA])
  AC_MSG_RESULT([(enabled)])
else
  PYTHIA_AVAILABLE_FLAG=".false."
  AC_MSG_CHECKING([for PYTHIA])
  AC_MSG_RESULT([(disabled)])
fi
AC_SUBST(PYTHIA_AVAILABLE_FLAG)

AM_CONDITIONAL([PYTHIA_AVAILABLE], [test "$enable_pythia" = "yes"])
])
