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
     AC_MSG_CHECKING([for get_recola_version_rcl in RECOLA])
     AC_LANG_PUSH([Fortran])
     recola_libdir=`dirname $RECOLA`
     RECOLA_DIR=$recola_libdir
     wo_recola_libdir="-L${recola_libdir}"
     wo_recola_ldflags="${lt_prog_compiler_wl_FC}-rpath,$RECOLA_DIR -L$RECOLA_DIR -lrecola -lcollier"
     wo_recola_ldflags_cc="${lt_prog_compiler_wl}-rpath,$RECOLA_DIR -L$RECOLA_DIR -lrecola -lcollier"
     wo_recola_includes="-I${recola_libdir}/../include"
     wo_recola_version=""
     save_LIBS="$LIBS"
     LIBS="${LIBS} ${wo_recola_ldflags} -lrecola -lcollier ${wo_recola_includes}"
     AC_LINK_IFELSE([dnl
        AC_LANG_PROGRAM([],[[
                use recola
		character(len=10) :: version
                call get_recola_version_rcl (version)
		print *, version
                ]])],
         [wo_recola_version=`./conftest | $SED -e 's/^[ \t]*//g;s/[ \t]*//g'`],
         [enable_recola="no"])
     AC_MSG_RESULT([$enable_recola])
     LIBS="$save_LIBS"
     if test "$enable_recola" = "no"; then
       AC_MSG_NOTICE([warning:  ********************************************************])
       AC_MSG_NOTICE([warning:  It seems your RECOLA was not compiled properly or       ])
       AC_MSG_NOTICE([warning:  compiled with a different FORTRAN compiler and you      ])
       AC_MSG_NOTICE([warning:  forgot to add the proper runtime to                     ])
       AC_MSG_NOTICE([warning:  LIBS / LD_LIBRARY_PATH. Disabling RECOLA support...     ])
       AC_MSG_NOTICE([warning:  ********************************************************])
       AC_MSG_CHECKING([for Recola])
       AC_MSG_RESULT([disabled])
     else
       if test "$wo_recola_version" = "1.0" || test "$wo_recola_version" = "1.1" || test "$wo_recola_version" = "1.2" || test "$wo_recola_version" = "1.3.1" || test "$wo_recola_version" = "1.3.2" || test "$wo_recola_version" = "1.3.3" || test "$wo_recola_version" = "1.4.1"  || test "$wo_recola_version" = "1.4.2" || test "$wo_recola_version" = "1.4.3" || test "$wo_recola_version" = "2.0.0" || test "$wo_recola_version" = 2.1.0 || test "$wo_recola_version" = 2.1.1; then
         AC_MSG_NOTICE([error: **************************************************])
         AC_MSG_NOTICE([error: RECOLA versions older than 1.4.4, or 2.0/2.1.0-1) ])
         AC_MSG_NOTICE([error: are not supported. RECOLA will be disabled.       ])
         AC_MSG_NOTICE([error: **************************************************])
         AC_MSG_CHECKING([for Recola])
         AC_MSG_RESULT([(disabled)])
         enable_recola="no"
         RECOLA_INTEGERVERSION=00000
         AC_SUBST([RECOLA_INTEGERVERSION])
       else 
         RECOLA_INCLUDES=$wo_recola_includes
         RECOLA_VERSION=$wo_recola_version
         AC_CACHE_VAL([wo_recola_cv_integer_version],
           [wo_recola_cv_integer_version="`echo "$wo_recola_version" | \
             $AWK 'NR==1 {
               changequote(<<,>>)dnl
                 split (<<$>>1, version, "[.+]+");
                 printf ("%d%02d%02d", version[1], version[2], version[3])}'`"
               changequote([,])])
         RECOLA_INTEGERVERSION=$wo_recola_cv_integer_version
         LDFLAGS_RECOLA=$wo_recola_ldflags_cc
         AC_SUBST([RECOLA_VERSION])
	 AC_SUBST([RECOLA_INTEGERVERSION])
         AC_SUBST([RECOLA_DIR])
         AC_MSG_CHECKING([for Recola version])
         AC_MSG_RESULT([$wo_recola_version])
       fi
     fi
     AC_LANG_POP()
  else
     AC_MSG_CHECKING([for Recola])
     AC_MSG_RESULT([(disabled)])
     enable_recola="no"
     RECOLA_INTEGERVERSION=00000
     AC_SUBST([RECOLA_INTEGERVERSION])
  fi
else
   AC_MSG_CHECKING([for Recola])
   AC_MSG_RESULT([(disabled)])
   enable_recola="no"
   RECOLA_INTEGERVERSION=00000
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


