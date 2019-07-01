dnl stdcpp.m4 -- take care of the C++ standard library
dnl if it is required
dnl

AC_DEFUN([WO_PROG_STDCPP],
[dnl

if test "$wo_require_stdcpp" = "yes"; then

  AC_REQUIRE([AC_PROG_CXX])

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

  AC_MSG_CHECKING([for LDFLAGS_STATIC: host system is $host_os: static flag])
  AC_MSG_RESULT([$wo_ldflags_stdcpp])

else

  AC_MSG_CHECKING([for LDFLAGS_STATIC:])
  AC_MSG_RESULT([(not needed)])

  unset wo_ldflags_stdcpp

fi

LDFLAGS_STATIC="$wo_ldflags_stdcpp"
AC_SUBST([LDFLAGS_STATIC])

])


