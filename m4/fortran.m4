dnl fortran.m4 -- Fortran compiler checks beyond Autoconf built-ins
dnl

dnl The standard Fortran compiler test is AC_PROG_FC.
dnl At the end FC, FCFLAGS and FCFLAGS_f90 are set, if successful.

### Determine vendor and version string.
AC_DEFUN([WO_FC_GET_VENDOR_AND_VERSION],
[dnl
AC_REQUIRE([AC_PROG_FC])

AC_CACHE_CHECK([the compiler ID string],
[wo_cv_fc_id_string],
[dnl
$FC -version >conftest.log 2>&1
$FC -V >>conftest.log 2>&1
$FC --version >>conftest.log 2>&1

wo_fc_grep_GFORTRAN=`grep -i 'GNU Fortran' conftest.log | head -1`
wo_fc_grep_G95=`grep -i 'g95' conftest.log | grep -i 'gcc' | head -1`
wo_fc_grep_NAG=`grep 'NAG' conftest.log | head -1`
wo_fc_grep_Intel=`grep 'Intel' conftest.log | head -1`
wo_fc_grep_Sun=`grep 'Sun' conftest.log | head -1`
wo_fc_grep_Lahey=`grep 'Lahey' conftest.log | head -1`
wo_fc_grep_PGI=`grep 'pgf' conftest.log | head -1`
wo_fc_grep_default=`cat conftest.log | head -1`

if test -n "$wo_fc_grep_GFORTRAN"; then
  wo_cv_fc_id_string=$wo_fc_grep_GFORTRAN
elif test -n "$wo_fc_grep_G95"; then
  wo_cv_fc_id_string=$wo_fc_grep_G95
elif test -n "$wo_fc_grep_NAG"; then
  wo_cv_fc_id_string=$wo_fc_grep_NAG
elif test -n "$wo_fc_grep_Intel"; then
  wo_cv_fc_id_string=$wo_fc_grep_Intel
elif test -n "$wo_fc_grep_Sun"; then
  wo_cv_fc_id_string=$wo_fc_grep_Sun
elif test -n "$wo_fc_grep_Lahey"; then
  wo_cv_fc_id_string=$wo_fc_grep_Lahey
elif test -n "$wo_fc_grep_PGI"; then
  wo_cv_fc_id_string=$wo_fc_grep_PGI
else
  wo_cv_fc_id_string=$wo_fc_grep_default
fi

rm -f conftest.log
])
FC_ID_STRING="$wo_cv_fc_id_string"
AC_SUBST([FC_ID_STRING])

AC_CACHE_CHECK([the compiler vendor],
[wo_cv_fc_vendor],
[dnl
if test -n "$wo_fc_grep_GFORTRAN"; then
  wo_cv_fc_vendor="gfortran"
elif test -n "$wo_fc_grep_G95"; then
  wo_cv_fc_vendor="g95"
elif test -n "$wo_fc_grep_NAG"; then
  wo_cv_fc_vendor="NAG"
elif test -n "$wo_fc_grep_Intel"; then
  wo_cv_fc_vendor="Intel"
elif test -n "$wo_fc_grep_Sun"; then
  wo_cv_fc_vendor="Sun"
elif test -n "$wo_fc_grep_Lahey"; then
  wo_cv_fc_vendor="Lahey"
elif test -n "$wo_fc_grep_PGI"; then
  wo_cv_fc_vendor="PGI"
else
  wo_cv_fc_vendor="unknown"
fi
])
FC_VENDOR="$wo_cv_fc_vendor"


AC_SUBST([FC_VENDOR])

AM_CONDITIONAL([FC_IS_GFORTRAN],
  [test "$FC_VENDOR" = gfortran])

AC_CACHE_CHECK([the compiler version],
[wo_cv_fc_version],
[dnl
case $FC_VENDOR in
gfortran)
  wo_cv_fc_version=[`echo $FC_ID_STRING | sed -e 's/.*\([0-9][0-9]*\.[0-9][0-9]*\.[0-9][0-9]*\).*/\1/'`]
  ;;
g95)
  wo_cv_fc_version=[`echo $FC_ID_STRING | sed -e 's/.*g95 \([0-9][0-9]*\.[0-9][0-9]*\).*$/\1/'`]
  ;;
NAG)
  wo_cv_fc_version=[`echo $FC_ID_STRING | sed -e 's/.* Release \([0-9][0-9]*\.[0-9][0-9]*.*$\)/\1/'`]
  ;;
Intel)
  wo_cv_fc_version=[`echo $FC_ID_STRING | sed -e 's/.* Version \([0-9][0-9]*\.[0-9][0-9]*\) .*/\1/'`]
  ;;
Sun)
  wo_cv_fc_version=[`echo $FC_ID_STRING | sed -e 's/.* Fortran 95 \([0-9][0-9]*\.[0-9][0-9]*\) .*/\1/'`]
  ;;
*)
  wo_cv_fc_version="unknown"
  ;;
esac
])
FC_VERSION="$wo_cv_fc_version"
AC_SUBST([FC_VERSION])
 
AC_CACHE_CHECK([the major version],
[wo_cv_fc_major_version],
[wo_cv_fc_major_version=[`echo $wo_cv_fc_version | sed -e 's/\([0-9][0-9]*\)\..*/\1/'`]
])
FC_MAJOR_VERSION="$wo_cv_fc_major_version"
AC_SUBST([FC_MAJOR_VERSION])


# case "$FC_VENDOR" in
# 
#   Intel)
# 
#     if test "$FC_MAJOR_VERSION" -lt 7; then
#       AC_MSG_ERROR([versions before 7.0 of the Intel Fortran compiler dnl
# are not supported, because they are too old and buggy.])
#     fi
# 
#     if test "$FC_MAJOR_VERSION" -lt 11; then
#       AC_MSG_ERROR([versions before 11.0 of the Intel Fortran compiler dnl
# do not support F2003 features.])
#     fi
# 
#     THO_FORTRAN_FIND_OPTION([FC_OPT], [$FC], [$FC_EXT], [-O3 -O])
#     THO_FORTRAN_FILTER_OPTIONS([FC_OPT], [$FC], [$FC_EXT], [-u])
#     THO_FORTRAN_FIND_OPTION([FC_PROF], [$FC], [$FC_EXT], [-p])
# 
#     if test "$FC_IFC_VERSION" -ge 8; then
#       FC_MDIR=-module
#       FC_WIDE=-132
#       FC_DUSTY=-FI
#     else
#       FC_MDIR=
#       FC_WIDE=-extend_source
#       FC_DUSTY=-FI
#     fi
#
#   Lahey)
#     THO_FORTRAN_FILTER_OPTIONS([FC_OPT], [$FC], [$FC_EXT],
#       [-O --tpp --nap --nchk --npca --nsav --ntrace dnl
#        --fc --in --nli --quiet --warn])
#     THO_FORTRAN_FIND_OPTION([FC_PROF], [$FC], [$FC_EXT], [-pg])
#     FC_MDIR=
#     FC_WIDE=--wide
#     FC_DUSTY=--fix
#     ;;
# 
#   NAG)
#     THO_FORTRAN_FIND_OPTION([FC_OPT], [$FC], [$FC_EXT],
#       ["-O3 -Oassumed=contig" -O3 -O])
#     THO_FORTRAN_FIND_OPTION([FC_PROF], [$FC], [$FC_EXT], [-pg])
#     FC_MDIR=-mdir
#     FC_WIDE=-132
#     FC_DUSTY="-dcfuns -fixed"
#     ;;
# 
#   Compaq)
#     THO_FORTRAN_FIND_OPTION([FC_OPT], [$FC], [$FC_EXT], [-O])
#     THO_FORTRAN_FIND_OPTION([FC_PROF], [$FC], [$FC_EXT], [-pg])
#     FC_MDIR=-module
#     FC_WIDE=-132
#     FC_DUSTY=-extend_source
#     ;;
# 
#   Sun)
#     THO_FORTRAN_FIND_OPTION([FC_OPT], [$FC], [$FC_EXT], [-O])
#     THO_FORTRAN_FIND_OPTION([FC_PROF], [$FC], [$FC_EXT], [-pg])
#     FC_MDIR=-moddir=
#     FC_MDIR_NOSPACE=yes
#     FC_WIDE=-e
#     FC_DUSTY=-fixed
#     ;;
# 
#   *)
#     THO_FORTRAN_FIND_OPTION([FC_OPT], [$FC], [$FC_EXT], [-O])
#     THO_FORTRAN_FIND_OPTION([FC_PROF], [$FC], [$FC_EXT], [-pg])
#     FC_MDIR=
#     FC_WIDE=-132
#     FC_DUSTY="-dcfuns -fixed"
#     ;;
# 
# esac

])
### end WO_FC_GET_VENDOR_AND_VERSION

### This is for deviations of the FORTRAN naming convention for modules from
### .mod

WO_FORTRAN90_MODULE_FILE([FC_MODULE_NAME], [FC_MODULE_EXT], [$FC], [$FC_EXT])

AC_SUBST([FC_MAKE_MODULE_NAME])
case "$FC_MODULE_NAME" in
  module_NAME)
    FC_MAKE_MODULE_NAME='$*.$(FC_MODULE_EXT)'
    ;;
  module_name)
    FC_MAKE_MODULE_NAME='"`echo $* | $(LOWERCASE)`".$(FC_MODULE_EXT)'
    ;;
  MODULE_NAME)
    FC_MAKE_MODULE_NAME='"`echo $* | $(UPPERCASE)`".$(FC_MODULE_EXT)'
    ;;
  conftest)
    FC_MAKE_MODULE_NAME='$*.$(FC_MODULE_EXT)'
    ;;
  *)
    ;;
esac

### Determine Fortran flags and file extensions
AC_DEFUN([WO_FC_PARAMETERS],
[dnl
AC_REQUIRE([AC_PROG_FC])
AC_REQUIRE([_LT_COMPILER_PIC])
AC_LANG([Fortran])

AC_MSG_CHECKING([for $FC flags])
AC_MSG_RESULT([$FCFLAGS])

AC_MSG_CHECKING([for $FC flag to produce position-independent code])
AC_MSG_RESULT([$lt_prog_compiler_pic_FC])
FCFLAGS_PIC=$lt_prog_compiler_pic_FC
AC_SUBST([FCFLAGS_PIC])

AC_MSG_CHECKING([for $FC source extension])
AC_MSG_RESULT([$ac_fc_srcext])
FC_SRC_EXT=$ac_fc_srcext
AC_SUBST([FC_SRC_EXT])

AC_MSG_CHECKING([for object file extension])
AC_MSG_RESULT([$ac_objext])
OBJ_EXT=$ac_objext
AC_SUBST([OBJ_EXT])
])
### end WO_FC_PARAMETERS


### Determine runtime libraries
### The standard check is insufficient for some compilers
AC_DEFUN([WO_FC_LIBRARY_LDFLAGS],
[dnl
AC_REQUIRE([AC_PROG_FC])
case "$FC" in
nagfor*)
  WO_NAGFOR_LIBRARY_LDFLAGS()
  ;;
*)
  AC_FC_LIBRARY_LDFLAGS
  ;;
esac
])

### Check the NAG Fortran compiler
### Use the '-dryrun' feature and extract the libraries from the link command
### Note that the linker is gcc, not ld
AC_DEFUN([WO_NAGFOR_LIBRARY_LDFLAGS],
[dnl
  AC_CACHE_CHECK([Fortran libraries of $FC],
  [wo_cv_fc_libs],
  [dnl
  if test -z "$FCLIBS"; then
    AC_LANG([Fortran])
    AC_LANG_CONFTEST([AC_LANG_PROGRAM([])])
    wo_save_fcflags=$FCFLAGS
    FCFLAGS="-dryrun"
    eval "set x $ac_link"
    echo "set x $ac_link"
    shift
    _AS_ECHO_LOG([$[*]])
    wo_nagfor_output=`eval $ac_link AS_MESSAGE_LOG_FD>&1 2>&1`
    echo "$wo_nagfor_output" >&AS_MESSAGE_LOG_FD
    FCFLAGS=$wo_save_fcflags
    wo_cv_fc_libs=`echo $wo_nagfor_output | sed -e 's/.* -o conftest \(.*\)$/\1/' | sed -e "s/conftest.$ac_objext //"`
  else
    wo_cv_fc_libs=$FCLIBS
  fi
  ])
  FCLIBS=$wo_cv_fc_libs
  AC_SUBST([FCLIBS])
])

### Check for basic F95 features
AC_DEFUN([WO_FC_CHECK_F95],
[dnl
AC_CACHE_CHECK([whether $FC supports Fortran 95 features],
[wo_cv_fc_supports_f95],
[dnl
AC_REQUIRE([AC_PROG_FC])
AC_LANG([Fortran])
AC_COMPILE_IFELSE([dnl
  program conftest
    integer, dimension(2) :: ii
    integer :: i
    type :: foo
       integer, pointer :: bar => null ()
    end type foo
    forall (i = 1:2)  ii(i) = i
  contains
    elemental function f(x)
      real, intent(in) :: x
      real :: f
      f = x
    end function f
    pure function g (x) result (gx)
      real, intent(in) :: x
      real :: gx
      gx = x
    end function g
  end program conftest
  ],
  [wo_cv_fc_supports_f95="yes"],
  [wo_cv_fc_supports_f95="no"])
])
FC_SUPPORTS_F95="$wo_cv_fc_supports_f95"
AC_SUBST([FC_SUPPORTS_F95])
if test "$FC_SUPPORTS_F95" = "no"; then
AC_MSG_NOTICE([error: ******************************************************************])
AC_MSG_NOTICE([error: Fortran compiler is not a genuine F95 compiler, configure aborted.])
AC_MSG_ERROR([******************************************************************])
fi])
### end WO_FC_CHECK_F95
 
### Check for the TR15581 extensions (allocatable subobjects)
AC_DEFUN([WO_FC_CHECK_TR15581],
[AC_CACHE_CHECK([whether $FC supports allocatable subobjects],
[wo_cv_fc_allocatable],
[dnl
AC_REQUIRE([AC_PROG_FC])
AC_LANG([Fortran])
AC_COMPILE_IFELSE([dnl
  program conftest
    type :: foo
       integer, dimension(:), allocatable :: bar
    end type foo
  end program conftest
  ],
  [wo_cv_fc_allocatable="yes"],
  [wo_cv_fc_allocatable="no"])
])
FC_SUPPORTS_ALLOCATABLE="$wo_cv_fc_allocatable"
AC_SUBST([FC_SUPPORTS_ALLOCATABLE])
])
### end WO_FC_CHECK_TR15581


### Check for allocatable scalars
AC_DEFUN([WO_FC_CHECK_ALLOCATABLE_SCALARS],
[AC_CACHE_CHECK([whether $FC supports allocatable scalars],
[wo_cv_fc_allocatable_scalars],
[dnl
AC_REQUIRE([AC_PROG_FC])
AC_LANG([Fortran])
AC_COMPILE_IFELSE([dnl
  program conftest
    type :: foo
       integer, allocatable :: bar
    end type foo
  end program conftest
  ],
  [wo_cv_fc_allocatable_scalars="yes"],
  [wo_cv_fc_allocatable_scalars="no"])
])
FC_SUPPORTS_ALLOCATABLE_SCALARS="$wo_cv_fc_allocatable"
AC_SUBST([FC_SUPPORTS_ALLOCATABLE_SCALARS])
])
### end WO_FC_CHECK_ALLOCATABLE_SCALARS


### Check for the C bindings extensions of Fortran 2003
AC_DEFUN([WO_FC_CHECK_C_BINDING],
[AC_CACHE_CHECK([whether $FC supports ISO C binding and standard numeric types],
[wo_cv_fc_c_binding],
[dnl
AC_REQUIRE([AC_PROG_FC])
AC_LANG([Fortran])
AC_COMPILE_IFELSE([dnl
  program conftest
    use iso_c_binding
    type, bind(c) :: t
       integer(c_int) :: i
       real(c_float) :: x1
       real(c_double) :: x2
       complex(c_float_complex) :: z1
       complex(c_double_complex) :: z2
    end type t
  end program conftest
  ],
  [wo_cv_fc_c_binding="yes"],
  [wo_cv_fc_c_binding="no"])
])
FC_SUPPORTS_C_BINDING="$wo_cv_fc_c_binding"
AC_SUBST([FC_SUPPORTS_C_BINDING])
if test "$FC_SUPPORTS_C_BINDING" = "no"; then
AC_MSG_NOTICE([error: *******************************************************************])
AC_MSG_NOTICE([error: Fortran compiler does not support ISO C binding, configure aborted.])
AC_MSG_ERROR([********************************************************************])
fi
])
### end WO_FC_CHECK_C_BINDING


### Check for procedure pointers
AC_DEFUN([WO_FC_CHECK_PROCEDURE_POINTERS],
[AC_CACHE_CHECK([whether $FC supports procedure pointers (F2003)],
[wo_cv_prog_f03_procedure_pointers],
[dnl
AC_REQUIRE([AC_PROG_FC])
AC_LANG([Fortran])
AC_COMPILE_IFELSE([dnl
  program conftest
    type :: foo
      procedure (proc_template), nopass, pointer :: proc => null ()
    end type foo
    abstract interface
      subroutine proc_template ()
      end subroutine proc_template
    end interface
  end program conftest
  ],
  [wo_cv_prog_f03_procedure_pointers="yes"],
  [wo_cv_prog_f03_procedure_pointers="no"])
])
FC_SUPPORTS_PROCEDURE_POINTERS="$wo_cv_prog_f03_procedure_pointers"
AC_SUBST([FC_SUPPORTS_PROCEDURE_POINTERS])
if test "$FC_SUPPORTS_PROCEDURE_POINTERS" = "no"; then
AC_MSG_NOTICE([error: ***************************************************************************])
AC_MSG_NOTICE([error: Fortran compiler does not understand procedure pointers, configure aborted.])
AC_MSG_ERROR([***************************************************************************])
fi])
### end WO_FC_CHECK_PROCEDURE_POINTERS


### Check for the OO extensions of Fortran 2003
AC_DEFUN([WO_FC_CHECK_OO_FEATURES],
[AC_CACHE_CHECK([whether $FC supports OO features (F2003)],
[wo_cv_prog_f03_oo_features],
[dnl
AC_REQUIRE([AC_PROG_FC])
AC_LANG([Fortran])
AC_COMPILE_IFELSE([dnl
  module conftest
    type, abstract :: foo
    contains
       procedure (proc_template), deferred :: proc
    end type foo
    type, extends (foo) :: foobar
    contains
       procedure :: proc
    end type foobar
    abstract interface
      subroutine proc_template (f)
        import foo
        class(foo), intent(inout) :: f
      end subroutine proc_template
    end interface
  contains
    subroutine proc (f)
      class(foobar), intent(inout) :: f
    end subroutine proc
  end module conftest
  program main
    use conftest
  end program main
  ],
  [wo_cv_prog_f03_oo_features="yes"],
  [wo_cv_prog_f03_oo_features="no"])
])
FC_SUPPORTS_OO_FEATURES="$wo_cv_prog_f03_oo_features"
AC_SUBST([FC_SUPPORTS_OO_FEATURES])
])
### end WO_FC_CHECK_OO_FEATURES


### Check for the command line interface of Fortran 2003
### We actually have to link in order to check availability
AC_DEFUN([WO_FC_CHECK_CMDLINE],
[AC_CACHE_CHECK([whether $FC interfaces the command line (F2003)],
  [wo_cv_fc_cmdline],
  [dnl
AC_REQUIRE([AC_PROG_FC])
AC_LANG([Fortran])
AC_LINK_IFELSE([dnl
  program conftest
    call get_command_argument (command_argument_count ())
  end program conftest
  ],
  [wo_cv_fc_cmdline="yes"],
  [wo_cv_fc_cmdline="no"])
])
FC_SUPPORTS_CMDLINE="$wo_cv_fc_cmdline"
AC_SUBST([FC_SUPPORTS_CMDLINE])
if test "$FC_SUPPORTS_CMDLINE" = "no"; then
AC_MSG_NOTICE([error: ******************************************************************])
AC_MSG_NOTICE([error: Fortran compiler does not support get_command_argument; configure aborted.])
AC_MSG_ERROR([******************************************************************])
fi
])
### end WO_FC_CHECK_CMDLINE

### Check whether we can access environment variables (2003 standard)
### We actually have to link in order to check availability
AC_DEFUN([WO_FC_CHECK_ENVVAR],
[AC_CACHE_CHECK([whether $FC provides access to environment variables (F2003)],
  [wo_cv_fc_envvar],
  [dnl
AC_REQUIRE([AC_PROG_FC])
AC_LANG([Fortran])
AC_LINK_IFELSE([dnl
  program conftest
    character(len=256) :: home
    call get_environment_variable ("HOME", home)
  end program conftest
  ],
  [wo_cv_fc_envvar="yes"],
  [wo_cv_fc_envvar="no"])
])
FC_SUPPORTS_ENVVAR="$wo_cv_fc_envvar"
AC_SUBST([FC_SUPPORTS_ENVVAR])
if test "$FC_SUPPORTS_PROCEDURE_ENVVAR" = "no"; then
AC_MSG_NOTICE([error: ***************************************************************************])
AC_MSG_NOTICE([error: Fortran compiler does not support get_environment_variable; configure aborted.])
AC_MSG_ERROR([***************************************************************************])
fi])
### end WO_FC_CHECK_ENVVAR

### Check for wrapping of linker flags 
### (nagfor 'feature': must be wrapped twice)
AC_DEFUN([WO_FC_CHECK_LDFLAGS_WRAPPING],
[AC_CACHE_CHECK([for wrapping of linker flags via -Wl],
  [wo_cv_fc_ldflags_wrapping],
  [dnl
AC_REQUIRE([AC_PROG_FC])
AC_LANG([Fortran])
ldflags_tmp=$LDFLAGS
LDFLAGS=-Wl,-rpath,/usr/lib
AC_LINK_IFELSE(AC_LANG_PROGRAM(),
  [wo_cv_fc_ldflags_wrapping="once"],
  [wo_cv_fc_ldflags_wrapping="unknown"])
if test "$wo_cv_fc_ldflags_wrapping" = "unknown"; then
  LDFLAGS=-Wl,-Wl,,-rpath,,/usr/lib  
  AC_LINK_IFELSE(AC_LANG_PROGRAM(),
    [wo_cv_fc_ldflags_wrapping="twice"])
fi
LDFLAGS=$ldflags_tmp
])
FC_LDFLAGS_WRAPPING="$wo_cv_fc_ldflags_wrapping"
AC_SUBST([FC_LDFLAGS_WRAPPING])
])
### end WO_FC_CHECK_LDFLAGS_WRAPPING

### Check for profiling support
AC_DEFUN([WO_FC_CHECK_PROFILING],
[AC_CACHE_CHECK([whether $FC supports profiling via -pg],
  [wo_cv_fc_profiling],
  [dnl
AC_REQUIRE([AC_PROG_FC])
AC_LANG([Fortran])
fcflags_tmp=$FCFLAGS
FCFLAGS="-pg $FCFLAGS"
rm -f gmon.out
AC_RUN_IFELSE([dnl
  program conftest
  end program conftest
  ],
  [dnl
  if test -f gmon.out; then
    wo_cv_fc_profiling="yes"
  else
    wo_cv_fc_profiling="no"
  fi],
  [wo_cv_fc_profiling="no"],
  [wo_cv_fc_profiling="maybe [cross-compiling]"])
rm -f gmon.out
FCFLAGS=$fcflags_tmp
])
FC_SUPPORTS_PROFILING="$wo_cv_fc_profiling"
AC_SUBST([FC_SUPPORTS_PROFILING])
])
### end WO_FC_CHECK_PROFILING

### Enable/disable profiling support
AC_DEFUN([WO_FC_SET_PROFILING],
[dnl
AC_REQUIRE([WO_FC_CHECK_PROFILING])
AC_ARG_ENABLE([profiling],
  [AS_HELP_STRING([--enable-fc-profiling],
    [use profiling for the Fortran code [[no]]])])
AC_CACHE_CHECK([the default setting for profiling], [wo_cv_fc_prof],
[dnl
if test "$FC_SUPPORTS_PROFILING" = "yes" -a "$profiling" = "yes"; then
  wo_cv_fc_prof="yes"
  FC_PROF="-pg"	
else
  wo_cv_fc_prof="no"
  FC_PROF=""
fi])
AC_SUBST(FC_PROF)
AM_CONDITIONAL([FC_PROF_SET],
	[test -n "$FC_PROF"])
])
### end WO_FC_SET_PROFILING

### Enable/disable impure Omega compilation
AC_DEFUN([WO_FC_SET_OMEGA_IMPURE],
[dnl
AC_REQUIRE([WO_FC_CHECK_F95])
AC_ARG_ENABLE([impure_omega],
  [AS_HELP_STRING([--enable-fc-impure],
    [compile Omega libraries impure [[no]]])])
AC_CACHE_CHECK([the default setting for impure omegalib], [wo_cv_fc_impure],
[dnl
if test "$impure_omega" = "yes" -o "$FC_SUPPORTS_F95" = "no"; then
  wo_cv_fc_impure="yes"
else 
  wo_cv_fc_impure="no"
fi])
AM_CONDITIONAL([FC_IMPURE],
     [test "$wo_cv_fc_impure" = "yes"])
])
### end WO_FC_OMEGA_IMPURE

### Check for quadruple precision support (real and complex!)
AC_DEFUN([WO_FC_CHECK_QUADRUPLE],
[dnl
AC_CACHE_CHECK([whether $FC permits quadruple real and complex],
  [wo_cv_fc_quadruple],
  [dnl
AC_REQUIRE([AC_PROG_FC])
AC_LANG([Fortran])
AC_COMPILE_IFELSE([dnl
  program conftest
     integer, parameter :: d=selected_real_kind(precision(1.)+1, range(1.)+1)
     integer, parameter :: q=selected_real_kind(precision(1._d)+1, range(1._d))
     real(kind=q) :: x
     complex(kind=q) :: z
  end program conftest
  ], 
  [wo_cv_fc_quadruple="yes"],
  [wo_cv_fc_quadruple="no"])
])
FC_SUPPORTS_QUADRUPLE="$wo_cv_fc_quadruple"
AC_SUBST([FC_SUPPORTS_QUADRUPLE])
])
### end WO_FC_CHECK_QUADRUPLE

### Check for C quadruple precision support (real and complex!)
AC_DEFUN([WO_FC_CHECK_QUADRUPLE_C],
[dnl
AC_CACHE_CHECK([whether $FC permits quadruple-precision C types],
  [wo_cv_fc_quadruple_c],
  [dnl
AC_REQUIRE([AC_PROG_FC])
AC_LANG([Fortran])
AC_COMPILE_IFELSE([dnl
  program conftest
     use iso_c_binding
     real(c_long_double) :: x
     complex(c_long_double_complex) :: z
  end program conftest
  ], 
  [wo_cv_fc_quadruple_c="yes"],
  [wo_cv_fc_quadruple_c="no"])
])
FC_SUPPORTS_QUADRUPLE_C="$wo_cv_fc_quadruple_c"
AC_SUBST([FC_SUPPORTS_QUADRUPLE_C])
])
### end WO_FC_CHECK_QUADRUPLE_C


### Enable/disable quadruple precision and set default precision
AC_DEFUN([WO_FC_SET_PRECISION],
[dnl
AC_REQUIRE([WO_FC_CHECK_QUADRUPLE])
AC_ARG_ENABLE([fc_quadruple],
  [AS_HELP_STRING([--enable-fc-quadruple],
    [use quadruple precision in Fortran code [[no]]])])
if test "$enable_fc_quadruple" = "yes"; then
  FC_QUAD_OR_SINGLE="quadruple"
else
  FC_QUAD_OR_SINGLE="single"
fi
AC_SUBST([FC_QUAD_OR_SINGLE])
AC_CACHE_CHECK([the default numeric precision], [wo_cv_fc_precision],
[dnl
if test "$FC_SUPPORTS_QUADRUPLE" = "yes" \
  -a "$FC_SUPPORTS_QUADRUPLE_C" = "yes" \
  -a "$enable_fc_quadruple" = "yes"; then
  wo_cv_fc_precision="quadruple"
  wo_cv_fc_precision_c="c_long_double"
else
  wo_cv_fc_precision="double"
  wo_cv_fc_precision_c="c_double"
fi
])
FC_PRECISION="$wo_cv_fc_precision"
FC_PRECISION_C="$wo_cv_fc_precision_c"
AC_SUBST(FC_PRECISION)
AC_SUBST(FC_PRECISION_C)
AM_CONDITIONAL([FC_QUAD],
     [test "$FC_PRECISION" = "quadruple"])
])
### end WO_FC_SET_PRECISION

### filename_case_conversion, define two variables LOWERCASE and 
### UPPERCASE for /bin/sh filters that convert strings to lower 
### and upper case, respectively
AC_DEFUN([WO_FC_FILENAME_CASE_CONVERSION],
[dnl
AC_SUBST([LOWERCASE])
AC_SUBST([UPPERCASE])
AC_PATH_PROGS(TR,tr)
AC_MSG_CHECKING([for case conversion])
if test -n "$TR"; then
  LOWERCASE="$TR A-Z a-z"
  UPPERCASE="$TR a-z A-Z"
  WO_FC_FILENAME_CASE_CONVERSION_TEST
fi
if test -n "$UPPERCASE" && test -n "$LOWERCASE"; then
  AC_MSG_RESULT([$TR works])
else
  LOWERCASE="$SED y/ABCDEFGHIJKLMNOPQRSTUVWXYZ/abcdefghijklmnopqrstuvwxyz/"
  UPPERCASE="$SED y/abcdefghijklmnopqrstuvwxyz/ABCDEFGHIJKLMNOPQRSTUVWXYZ/"
  WO_FC_FILENAME_CASE_CONVERSION_TEST
  if test -n "$UPPERCASE" && test -n "$LOWERCASE"; then
    AC_MSG_RESULT([$SED works])
  fi
fi])
### end WO_FC_FILE_CASE_CONVERSION
dnl
AC_DEFUN([WO_FC_FILENAME_CASE_CONVERSION_TEST],
[dnl
if test "`echo fOo | $LOWERCASE`" != "foo"; then
  LOWERCASE=""
fi
if test "`echo fOo | $UPPERCASE`" != "FOO"; then
  UPPERCASE=""
fi])

dnl
dnl --------------------------------------------------------------------
dnl
dnl FC_TEST_OPTION(VARIABLE, COMPILER, EXTENSION, OPTION)
dnl
dnl   Test whether the COMPILER accepts the OPTION (using EXTENSION
dnl   for the test source).  If so, the VARIABLE will be set to OPTION.
dnl
AC_DEFUN([FC_TEST_OPTION],
[if test -n "$2"; then
   COMPILE_FC([$1], [$2 $4], [$3], [], [$4], [])
fi])

dnl
dnl --------------------------------------------------------------------
dnl
dnl COMPILE_FC(VARIABLE, COMPILER, EXTENSION, MODULE,
dnl                       VALUE_SUCCESS, VALUE_FAILURE, KEEP)
dnl
AC_DEFUN([COMPILE_FC],
[cat >conftest.$3 <<__END__
$4
program conftest
  print *, 42
end program conftest
__END__
$2 -o conftest conftest.$3 >/dev/null 2>&1
./conftest >conftest.out 2>/dev/null
if test 42 = "`sed 's/ //g' conftest.out`"; then
  $1="$5"
else
  $1="$6"
fi
if test -z "$7"; then
  rm -rf conftest* CONFTEST*
fi])


dnl --------------------------------------------------------------------
dnl
dnl FC_TEST_EXTENSION(VARIABLE, COMPILER, EXTENSION)
dnl

AC_DEFUN([FC_TEST_EXTENSION],
[AC_SUBST([$1])
if test -n "$2"; then
   COMPILE_FC([$1], [$2], [$3], [], [$3], [])
fi])

dnl
dnl --------------------------------------------------------------------
dnl
dnl FC_FIND_EXTENSION(VARIABLE, COMPILER, EXTENSIONS)
dnl
AC_DEFUN([FC_FIND_EXTENSION],
[AC_SUBST([$1])
for ext in $3; do
   AC_MSG_CHECKING([whether $2 supports .$ext])
   FC_TEST_EXTENSION([$1], [$2], [$ext])
   if test -n "[$]$1"; then
      AC_MSG_RESULT([yes]);
      FC_COMPILES="yes"
      break 
   else
      AC_MSG_RESULT([no])
   fi
done
AC_SUBST([FC_COMPILES])
if test "$FC_COMPILES" != "yes"; then
AC_MSG_NOTICE([error: **************************************************************])
AC_MSG_NOTICE([error: Fortran compiler cannot create executables, configure aborted.])
AC_MSG_ERROR([**************************************************************])
fi]
)

dnl
dnl
dnl --------------------------------------------------------------------
dnl
dnl FC_FIND_OPTION(VARIABLE, COMPILER, EXTENSION, OPTIONS)
dnl
dnl   Append the first accepted option from OPTIONS to VARIABLE.
dnl
AC_DEFUN([FC_FIND_OPTION],
[AC_SUBST([$1])
for option in $4; do
   AC_MSG_CHECKING([whether '$2' accepts $option])
   FC_TEST_OPTION([tmp_$1], [$2], [$3], [$option])
   if test -n "[$]tmp_$1"; then
      $1="[$]$1 [$]tmp_$1"
      AC_MSG_RESULT([yes])
      break
   else
      AC_MSG_RESULT([no])
   fi
done])

dnl --------------------------------------------------------------------
dnl
dnl FC_FILTER_OPTIONS(VARIABLE, COMPILER, EXTENSION, OPTIONS)
dnl
dnl   Append all accepted options from OPTIONS to VARIABLE.
dnl
AC_DEFUN([FC_FILTER_OPTIONS],
[AC_SUBST([$1])
for option in $4; do
   AC_MSG_CHECKING([whether '$2' accepts $option])
   FC_TEST_OPTION([tmp_$1], [$2], [$3], [$option])
   if test -n "[$]tmp_$1"; then
      $1="[$]$1 [$]tmp_$1"
      AC_MSG_RESULT([yes])
   else
      AC_MSG_RESULT([no])
   fi
done])

dnl
dnl --------------------------------------------------------------------
dnl
dnl FC_MODULE_FILE(NAME, EXTENSION, COMPILER, EXTENSION)
dnl
AC_DEFUN([FC_MODULE_FILE],
[AC_SUBST([$1])
AC_SUBST([$2])
AC_MSG_CHECKING([for Fortran90 module file naming convention])
COMPILE_FC([tho_result], [$3], [$4],
  [module module_NAME
     implicit none
     integer, parameter, public :: forty_two = 42
   end module module_NAME], [ok], [], [KEEP])
if test -n "$tho_result"; then
  $1=unknown
  $2=unknown
  for name in module_NAME module_name MODULE_NAME conftest; do
    for ext in m mod M MOD d D; do
      if test -f "$name.$ext"; then
        $1="$name"
        $2="$ext"
        break 2
      fi
    done
  done
  AC_MSG_RESULT([name: [$]$1, extension: .[$]$2 ])
else
  $1=""
  $2=""
  AC_MSG_RESULT([compiler failed])
fi
rm -rf conftest* CONFTEST* module_name* module_NAME* MODULE_NAME*])
