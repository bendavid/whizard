dnl noweb.m4 -- checks for NOWEB programs
dnl

### Determine paths to noweb components
AC_DEFUN([WO_PROG_NOWEB],
[dnl
AC_ARG_ENABLE([noweb],
  [AC_HELP_STRING([--disable-noweb],
    [disable the noweb programs, even if available [[no]]])])
if test "$enable_noweb" != "no"; then
AC_PATH_PROG([NOTANGLE], [notangle])
AC_PATH_PROG([CPIF], [cpif])
AC_PATH_PROG([NOWEAVE], [noweave])		
fi
AC_SUBST([NOTANGLE])
AC_SUBST([NOWEAVE])
AC_SUBST([CPIF])
AM_CONDITIONAL([NOWEB_AVAILABLE],
  [test "$enable_noweb" != "no" -a -n "$NOTANGLE" -a -n "$CPIF" -a -n "$NOWEAVE"])
])



