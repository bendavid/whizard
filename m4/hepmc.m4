dnl hepmc.m4 -- checks for HepMC library
dnl

### Determine paths to HEPMC components
### If successful, set the conditional HEPMC_AVAILABLE
### Also: HEPMC_VERSION HEPMC_INCLUDES LDFLAGS_HEPMC
AC_DEFUN([WO_PROG_HEPMC],
[dnl
AC_REQUIRE([AC_PROG_CXX])
AC_REQUIRE([AC_PROG_FC])

AC_ARG_ENABLE([hepmc],
  [AS_HELP_STRING([--enable-hepmc],
    [enable HepMC for handling event data [[yes]]])],
  [], [enable_hepmc="yes"])

if test -n "$HEPMC_DIR"; then
  wo_hepmc_includes="-I$HEPMC_DIR/include"
fi

if test "$enable_hepmc" = "yes"; then
  AC_MSG_CHECKING([the HepMC version])
  AC_LANG([C++])
  wo_cxxflags_tmp=$CXXFLAGS
  CXXFLAGS="$CXXFLAGS $wo_hepmc_includes"
  AC_LINK_IFELSE([dnl
    AC_LANG_PROGRAM([[#include "HepMC/Version.h"]],
      [[std::cout << HepMC::versionName();]])],
    [dnl
    wk_hepmc_version=`./conftest`
    AC_MSG_RESULT([$wk_hepmc_version])],
    [dnl
    AC_MSG_RESULT([unknown])
    enable_hepmc="no"])
  CXXFLAGS=$wo_cxxflags_tmp
fi
HEPMC_VERSION=$wk_hepmc_version
AC_SUBST([HEPMC_VERSION])

wo_hepmc_ldflags="-lHepMC"

if test "$enable_hepmc" = "yes"; then
  wo_require_stdcpp="yes"
  AC_MSG_CHECKING([for GenEvent class in -lHepMC])
  if test -n "$HEPMC_DIR"; then
    wo_hepmc_ldflags="-L$HEPMC_DIR/lib $wo_hepmc_ldflags"
  fi
  wo_libs_tmp=$LIBS
  LIBS="$wo_hepmc_ldflags $wo_libs_tmp"
  AC_LANG([C++])
  wo_cxxflags_tmp=$CXXFLAGS
  CXXFLAGS="$CXXFLAGS $wo_hepmc_includes"
  AC_LINK_IFELSE([dnl
    AC_LANG_PROGRAM([[#include "HepMC/GenEvent.h"]],
      [[using namespace HepMC;  GenEvent* evt = new GenEvent();]])],
    [],
    [enable_hepmc="no"])
  AC_MSG_RESULT([$enable_hepmc])
  CXXFLAGS=$wo_cxxflags_tmp
  LIBS=$wo_libs_tmp
else
  AC_MSG_CHECKING([for HepMC])
  AC_MSG_RESULT([(disabled)])
fi

if test "$enable_hepmc" = "yes"; then
  HEPMC_INCLUDES=$wo_hepmc_includes
  LDFLAGS_HEPMC=$wo_hepmc_ldflags
fi

HEPMC_AVAILABLE_FLAG=$enable_hepmc

AC_SUBST([HEPMC_INCLUDES])
AC_SUBST([LDFLAGS_HEPMC])
AC_SUBST([HEPMC_AVAILABLE_FLAG])

AM_CONDITIONAL([HEPMC_AVAILABLE], [test "$enable_hepmc" = "yes"])
])
