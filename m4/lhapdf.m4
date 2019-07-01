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

    AC_MSG_CHECKING([the standard PDF sets])
    if test -f "$LHAPDF_PDFSETS_PATH/cteq61.LHpdf" -a -f "$LHAPDF_PDFSETS_PATH/cteq5l.LHgrid" -a -f "$LHAPDF_PDFSETS_PATH/GSG961.LHgrid" -a -f "$LHAPDF_PDFSETS_PATH/cteq6ll.LHpdf"; then
       AC_MSG_RESULT([ all standard PDF sets installed])
    else	  
       AC_MSG_RESULT([ not all standard PDF sets installed])     
       AC_MSG_NOTICE([error: *************************************************************])
       AC_MSG_NOTICE([error: LHAPDF standard PDF sets not installed, please install these ])
       AC_MSG_NOTICE([error:    PDF sets: cteq61.LHpdf, cteq6ll.LHpdf, cteq5l.LHgrid,     ])
       AC_MSG_NOTICE([error:	GSG961.LHgrid.     ])
       AC_MSG_NOTICE([error: *************************************************************])
       enable_lhapdf="no"
       AC_MSG_CHECKING([for LHAPDF])
       AC_MSG_RESULT([(disabled)])
    fi
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

dnl LHAPDF requires the STD C++ library, when linking statically
if test "$enable_lhapdf" = "yes"; then
   ### Checking for static C++ libraries for the static version
   ### This is only necessary for MAC OS X and BSD-like OS
   case $host in
     *-darwin*)
        case "$XCODE_VERSION" in
          1.*|2.*|3.*)	  
   	wo_ldflags_stdcpp="-lstdc++-static" ;;
          *)
           wo_ldflags_stdcpp="-lstdc++" ;;
        esac ;;
     *-*-freebsd2*|*-*-freebsd3.0*|*-*-freebsdelf3.0*)
	wo_ldflags_stdcpp="-lstdc++-static" ;;
     *)
        wo_ldflags_stdcpp="-lstdc++" ;;
  esac
  AC_MSG_CHECKING([for wo_ldflags_stdcpp: host system is $host_os: static flag])
  AC_MSG_RESULT([$wo_ldflags_stdcpp])
fi

if test "$enable_lhapdf" = "yes"; then
  wo_lhapdf_libdir="-L$LHAPDF_ROOT/lib"
  AC_LANG([Fortran])
  AC_CHECK_LIB([LHAPDF],[getxminm],[LDFLAGS_LHAPDF="$wo_lhapdf_libdir -lLHAPDF"],
    [dnl
      AC_MSG_NOTICE([warning:  ********************************************************])
      AC_MSG_NOTICE([warning:  Either your LHAPDF version is too old (you need 5.3.0 or])
      AC_MSG_NOTICE([warning:  higher), or LHAPDF was compiled with a different FORTRAN])
      AC_MSG_NOTICE([warning:  compiler you and forgot to add the proper runtime to    ])
      AC_MSG_NOTICE([warning:  LIBS / LD_LIBRARY_PATH. Disabling LHAPDF support...     ])
      AC_MSG_NOTICE([warning:  ********************************************************])
      enable_lhapdf=no
    ],[$wo_lhapdf_libdir])
fi
AC_SUBST(LDFLAGS_LHAPDF)


dnl Determine whether we need to stub photon-as-parton related bits of LHAPDF
if test "$enable_lhapdf" = "yes"; then
  AC_LANG([Fortran])
  AC_CHECK_LIB([LHAPDF],[has_photon],[test],[dnl
     AC_MSG_NOTICE([warning:  ********************************************************])
     AC_MSG_NOTICE([warning:  Your LHAPDF version is not supported for PDF sets like  ])
     AC_MSG_NOTICE([warning:  MRTS2004QED which include the photon as a parton ---    ])
     AC_MSG_NOTICE([warning:  don't try to use those!                                 ])
     AC_MSG_NOTICE([warning:  ********************************************************])
     LHAPDF_HAS_PHOTON_DUMMY=true
   ],[$wo_lhapdf_libdir])
fi

if test "$enable_lhapdf" = "yes"; then
  LHAPDF_AVAILABLE_FLAG=".true."
else
  LHAPDF_AVAILABLE_FLAG=".false."
fi
AC_SUBST(LHAPDF_AVAILABLE_FLAG)

AM_CONDITIONAL([LHAPDF_AVAILABLE], [test "$enable_lhapdf" = "yes"])
AM_CONDITIONAL([LHAPDF_FULL_DUMMY], [test "$enable_lhapdf" = "no"])
AM_CONDITIONAL([LHAPDF_HAS_PHOTON_DUMMY], [test -n "$LHAPDF_HAS_PHOTON_DUMMY"])
AM_CONDITIONAL([LHAPDF_DUMMY], [
   test -n "$LHAPDF_HAS_PHOTON_DUMMY" || \
   test "$enable_lhapdf" = "no" dnl
 ])

])
