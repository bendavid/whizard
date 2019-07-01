dnl recola.m4 -- checks for Recola package
dnl

AC_DEFUN([WO_PROG_RECOLA],
[dnl
AC_REQUIRE([AC_PROG_FC])

AC_ARG_ENABLE([recola],
  [AS_HELP_STRING([--enable-recola],
     [(experimental) enable Recola for NLO matrix elements [[no]]])],
  [], [enable_recola="no"])

AC_ARG_WITH([recola],
  [AS_HELP_STRING([--with-recola=dir],
	  	  [assume the given directory for Recola])])

if test "$enable_recola" = "yes"; then

  if test -n "$with_recola"; then
    WO_PATH_LIB(RECOLA, [recola], [librecola.${SHRLIB_EXT}], ${with_recola})
  else
    WO_PATH_LIB(RECOLA, [recola], [librecola.${SHRLIB_EXT}], $LD_LIBRARY_PATH)
  fi
  if test "$RECOLA" != "no"; then
     AC_MSG_CHECKING([for openOpenput in RECOLA])
     AC_LANG_PUSH([Fortran])
     recola_libdir=`dirname $RECOLA`
     RECOLA_DIR=$recola_libdir
     wo_recola_libdir="-L${recola_libdir}"
     wo_recola_ldflags="-Wl,-rpath,$RECOLA_DIR -L$RECOLA_DIR -lrecola"
     wo_recola_includes="-I${recola_libdir}/modules"
     wo_recola_version=""
     save_LIBS="$LIBS"
     LIBS="${LIBS} ${wo_recola_libdir} -lrecola ${wo_recola_includes}"
     AC_LINK_IFELSE([dnl
	AC_LANG_PROGRAM([],[[
		use globals_rcl
		call openOutput
		]])],
         [wo_recola_version=`./conftest | $GREP 'Version' | $SED 's/.* Version //g'`],
	 [enable_recola="no"])
     AC_MSG_RESULT([$enable_recola])
     if test "$enable_recola" = "no"; then
       LIBS="$save_LIBS"
       AC_MSG_NOTICE([warning:  ********************************************************])
       AC_MSG_NOTICE([warning:  It seems your RECOLA was not compiled properly or       ])
       AC_MSG_NOTICE([warning:  compiled with a different FORTRAN compiler and you      ])
       AC_MSG_NOTICE([warning:  forgot to add the proper runtime to                     ])
       AC_MSG_NOTICE([warning:  LIBS / LD_LIBRARY_PATH. Disabling RECOLA support...     ])
       AC_MSG_NOTICE([warning:  ********************************************************])
       AC_MSG_CHECKING([for Recola])
       AC_MSG_RESULT([disabled])
     else
       RECOLA_INCLUDES=$wo_recola_includes
       RECOLA_VERSION=$wo_recola_version
       LDFLAGS_RECOLA=$wo_recola_ldflags
       AC_SUBST([RECOLA_VERSION])
       AC_SUBST([RECOLA_DIR])
       AC_MSG_CHECKING([for Recola version])
       AC_MSG_RESULT([$wo_recola_version])
     fi
     AC_LANG_POP()
  else
     AC_MSG_CHECKING([for Recola])
     AC_MSG_RESULT([(disabled)])
     enable_recola="no"
  fi
else
   AC_MSG_CHECKING([for Recola])
   AC_MSG_RESULT([(disabled)])
fi

AC_SUBST([RECOLA_INCLUDES])
AC_SUBST([LDFLAGS_RECOLA])

if test "$enable_recola" = "yes"; then
   RECOLA_AVAILABLE_FLAG=".true."
else
   RECOLA_AVAILABLE_FLAG=".false."
fi
AC_SUBST([RECOLA_AVAILABLE_FLAG])

AM_CONDITIONAL([RECOLA_AVAILABLE], [test "$enable_recola" = "yes"])

]) dnl WO_PROG_RECOLA


