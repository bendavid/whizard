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
$FC -V >conftest.log 2>&1
$FC -version >>conftest.log 2>&1
$FC --version >>conftest.log 2>&1

wo_fc_grep_GFORTRAN=`$GREP -i 'GNU Fortran' conftest.log | head -1`
wo_fc_grep_G95=`$GREP -i 'g95' conftest.log | $GREP -i 'gcc' | head -1`
wo_fc_grep_NAG=`$GREP 'NAG' conftest.log | head -1`
wo_fc_grep_Intel=`$GREP 'IFORT' conftest.log | head -1`
wo_fc_grep_Sun=`$GREP 'Sun' conftest.log | head -1`
wo_fc_grep_Lahey=`$GREP 'Lahey' conftest.log | head -1`
wo_fc_grep_PGF90=`$GREP 'pgf90' conftest.log | head -1`
wo_fc_grep_PGF95=`$GREP 'pgf95' conftest.log | head -1`
wo_fc_grep_PGHPF=`$GREP 'pghpf' conftest.log | head -1`
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
elif test -n "$wo_fc_grep_PGF90"; then
  wo_cv_fc_id_string=$wo_fc_grep_PGF90
elif test -n "$wo_fc_grep_PGF95"; then
  wo_cv_fc_id_string=$wo_fc_grep_PGF95
elif test -n "$wo_fc_grep_PGHPF"; then
  wo_cv_fc_id_string=$wo_fc_grep_PGHPF
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
elif test -n "$wo_fc_grep_PGF90"; then
  wo_cv_fc_vendor="PGI"
elif test -n "$wo_fc_grep_PGF95"; then
  wo_cv_fc_vendor="PGI"
elif test -n "$wo_fc_grep_PGHPF"; then
  wo_cv_fc_vendor="PGI"
else
  wo_cv_fc_vendor="unknown"
fi
])
FC_VENDOR="$wo_cv_fc_vendor"


AC_SUBST([FC_VENDOR])

AM_CONDITIONAL([FC_IS_GFORTRAN],
  [test "$FC_VENDOR" = gfortran])

AM_CONDITIONAL([FC_IS_NAG],
  [test "$FC_VENDOR" = NAG])

AC_CACHE_CHECK([the compiler version],
[wo_cv_fc_version],
[dnl
case $FC_VENDOR in
gfortran)
  wo_cv_fc_version=[`echo $FC_ID_STRING | $SED -e 's/.*\([0-9][0-9]*\.[0-9][0-9]*\.[0-9][0-9]*\).*/\1/'`]
  ;;
g95)
  wo_cv_fc_version=[`echo $FC_ID_STRING | $SED -e 's/.*g95 \([0-9][0-9]*\.[0-9][0-9]*\).*$/\1/'`]
  ;;
NAG)
  wo_cv_fc_version=[`echo $FC_ID_STRING | $SED -e 's/.* Release \([0-9][0-9]*\.[0-9][0-9]*.*$\)/\1/'`]
  ;;
Intel)
  wo_cv_fc_version=[`echo $FC_ID_STRING | $SED -e 's/[a-zA-Z\(\)]//g;s/[0-9]\{8\}$//g'`]
  ;;
Sun)
  wo_cv_fc_version=[`echo $FC_ID_STRING | $SED -e 's/.* Fortran 95 \([0-9][0-9]*\.[0-9][0-9]*\) .*/\1/'`]
  ;;
PGI)
  wo_cv_fc_version=[`echo $FC_ID_STRING | $SED -e 's/[a-zA-Z\(\)]//g;s/^[0-9]\{2\}//g;s/32.*\|64.*//g'`]
  ;;
*)
  wo_cv_fc_version="unknown"
  ;;
esac
])
FC_VERSION="$wo_cv_fc_version"
AC_SUBST([FC_VERSION])

### Catch gfortran memory leak in user structure function test
AM_CONDITIONAL([FC_IS_GFORTRAN_450],
  [test "$wo_cv_fc_vendor" = "gfortran" -a "$wo_cv_fc_version" = "4.5.0" || test "$wo_cv_fc_vendor" = "gfortran" -a "$wo_cv_fc_version" = "4.5.1" || test "$wo_cv_fc_vendor" = "gfortran" -a "$wo_cv_fc_version" = "4.5.2" || test "$wo_cv_fc_vendor" = "gfortran" -a "$wo_cv_fc_version" = "4.5.3" || test "$wo_cv_fc_vendor" = "gfortran" -a "$wo_cv_fc_version" = "4.5.4"])
 
AC_CACHE_CHECK([the major version],
[wo_cv_fc_major_version],
[wo_cv_fc_major_version=[`echo $wo_cv_fc_version | $SED -e 's/\([0-9][0-9]*\)\..*/\1/'`]
])
FC_MAJOR_VERSION="$wo_cv_fc_major_version"
AC_SUBST([FC_MAJOR_VERSION])
])
### end WO_FC_GET_VENDOR_AND_VERSION


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
    wo_cv_fc_libs=`echo $wo_nagfor_output | $SED -e 's/.* -o conftest \(.*\)$/\1/' | $SED -e "s/conftest.$ac_objext //"`
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
if test "$FC_SUPPORTS_ALLOCATABLE" = "no"; then
AC_MSG_NOTICE([error: ****************************************************************************])
AC_MSG_NOTICE([error: Fortran compiler does not support allocatable structures, configure aborted.])
AC_MSG_ERROR([****************************************************************************])
fi])
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
if test "$FC_SUPPORTS_ENVVAR" = "no"; then
AC_MSG_NOTICE([error: ***************************************************************************])
AC_MSG_NOTICE([error: Fortran compiler does not support get_environment_variable; configure aborted.])
AC_MSG_ERROR([***************************************************************************])
fi])
### end WO_FC_CHECK_ENVVAR

### Check whether the flush statement is supported
AC_DEFUN([WO_FC_CHECK_FLUSH],
[AC_CACHE_CHECK([whether $FC supports the flush statement (F2003)],
  [wo_cv_fc_flush],
  [dnl
AC_REQUIRE([AC_PROG_FC])
AC_LANG([Fortran])
AC_COMPILE_IFELSE([dnl
  program conftest
  implicit none
    integer, parameter :: u=50
    open (u, action="readwrite", status="scratch")
    write (u, *) "test"
    flush (u)
    close (u)
  end program conftest
  ],
  [wo_cv_fc_flush="yes"],
  [wo_cv_fc_flush="no"])
])
FC_SUPPORTS_FLUSH="$wo_cv_fc_flush"
AC_SUBST([FC_SUPPORTS_FLUSH])
if test "$FC_SUPPORTS_FLUSH" = "no"; then
AC_MSG_NOTICE([error: ***************************************************************************])
AC_MSG_NOTICE([error: Fortran compiler does not support the flush statement; configure aborted.])
AC_MSG_ERROR([***************************************************************************])
fi])
### end WO_FC_CHECK_FLUSH

### Check for iso_fortran_env
AC_DEFUN([WO_FC_CHECK_ISO_FORTRAN_ENV],
  [AC_ARG_ENABLE([iso_fortran_env],
    [AC_HELP_STRING([--disable-iso_fortran_env],
      [disable use of iso_fortran_env, even if the compiler supports it]
    )],
    [], [enable_iso_fortran_env=yes]
  )
  if test "$enable_iso_fortran_env" = yes; then
    AC_CACHE_CHECK([whether $FC supports iso_fortran_env (F2003)],
      [wo_cv_fc_iso_fortran_env],
       AC_REQUIRE([AC_PROG_FC])
       AC_LANG([Fortran])
      [AC_LINK_IFELSE(
        [dnl
        program conftest
        use iso_fortran_env
        implicit none
          integer :: i
          i = input_unit
          i = output_unit
          i = error_unit
          i = iostat_end
          i = iostat_eor
        end program conftest
        ], [wo_cv_fc_iso_fortran_env=yes], [wo_cv_fc_iso_fortran_env=no]
      )]
    )
    test "$wo_cv_fc_iso_fortran_env" = no && iso_fortran_env_stub=yes
  else
    AC_CHECKING([whether $FC supports iso_fortran_env (F2003)... disabled])
    iso_fortran_env_stub=yes
  fi
  AM_CONDITIONAL([ISO_FORTRAN_ENV_STUB], [test -n "$iso_fortran_env_stub"])
  ]
)

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

### Check for OpenMP support
AC_DEFUN([WO_FC_CHECK_OPENMP],
[AC_CACHE_CHECK([whether $FC supports OpenMP],
  [wo_cv_fc_openmp],
  [dnl
AC_REQUIRE([AC_PROG_FC])
AC_LANG([Fortran])
case $FC_VENDOR in
gfortran)
  wo_cv_fc_openmp="yes"
  wo_cv_fcflags_openmp="-fopenmp"
  wo_cv_fc_openmp_header="use omp_lib"
  ;;
NAG)
  wo_cv_fc_openmp="no"
  ;;
Intel)
  wo_cv_fc_openmp="yes"
  wo_cv_fcflags_openmp="-openmp"
  wo_cv_fc_openmp_header="use omp_lib"
  ;;
PGI)
  wo_cv_fc_openmp="yes"
  wo_cv_fcflags_openmp="-mp"
  wo_cv_fc_openmp_header=""
  ;;
*)
  wo_cv_fc_openmp="no"
  ;;
esac
if test "$wo_cv_fc_openmp"="yes"; then
fcflags_tmp=$FCFLAGS
FCFLAGS="$wo_cv_fcflags_openmp $FCFLAGS"
unset OMP_NUM_THREADS
AC_RUN_IFELSE([dnl
  program conftest
  $wo_cv_fc_openmp_header
  open (11, file="conftest.out", action="write", status="replace")
  write (11, "(I0)") omp_get_max_threads ()
  close (11)
  end program conftest
  ],
  [dnl
  wo_cv_fc_openmp="yes"
  wo_cv_fc_openmp_thread_limit=`cat conftest.out`],
  [wo_cv_fc_openmp="no"],
  [wo_cv_fc_openmp="maybe [cross-compiling]"])
FCFLAGS=$fcflags_tmp
fi
])
if test "$wo_cv_fc_openmp" = "yes"; then
AC_CACHE_CHECK([the default number of threads used by OpenMP],
[wo_cv_fc_openmp_thread_limit])
fi
FC_SUPPORTS_OPENMP="$wo_cv_fc_openmp"
AC_SUBST([FC_SUPPORTS_OPENMP])
])
### end WO_FC_CHECK_OPENMP

### Enable/disable OpenMP support
AC_DEFUN([WO_FC_SET_OPENMP],
[dnl
AC_REQUIRE([WO_FC_CHECK_OPENMP])
AC_ARG_ENABLE([fc_openmp],
  [AS_HELP_STRING([--enable-fc-openmp],
    [use OpenMP for the Fortran code [[no]]])])
AC_CACHE_CHECK([whether OpenMP is activated], [wo_cv_fc_use_openmp],
[dnl
if test "$FC_SUPPORTS_OPENMP" = "yes" -a "$enable_fc_openmp" = "yes"; then
  wo_cv_fc_use_openmp="yes"
else
  wo_cv_fc_use_openmp="no"
fi])
AM_CONDITIONAL([FC_USE_OPENMP],
	[test "$wo_cv_fc_use_openmp" = "yes"])
AM_COND_IF([FC_USE_OPENMP],
[FC_OPENMP_ON=""
FC_OPENMP_OFF="!"
FCFLAGS_OPENMP="$wo_cv_fcflags_openmp"
FC_OPENMP_HEADER="$wo_cv_fc_openmp_header"
FC_OPENMP_DEFAULT_MAX_THREADS="$wo_cv_fc_openmp_thread_limit"
],
[FC_OPENMP_ON="!"
FC_OPENMP_OFF=""
FC_OPENMP_DEFAULT_MAX_THREADS="1"
])
AC_SUBST([FC_OPENMP_ON])
AC_SUBST([FC_OPENMP_OFF])
AC_SUBST([FCFLAGS_OPENMP])
AC_SUBST([FC_OPENMP_HEADER])
AC_SUBST([FC_OPENMP_DEFAULT_MAX_THREADS])
])
### end WO_FC_SET_OPENMP

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
AC_ARG_ENABLE([fc_profiling],
  [AS_HELP_STRING([--enable-fc-profiling],
    [use profiling for the Fortran code [[no]]])])
AC_CACHE_CHECK([whether profiling is activated], [wo_cv_fc_prof],
[dnl
if test "$FC_SUPPORTS_PROFILING" = "yes" -a "$enable_fc_profiling" = "yes"; then
  wo_cv_fc_prof="yes"
  FCFLAGS_PROFILING="-pg"	
else
  wo_cv_fc_prof="no"
  FCFLAGS_PROFILING=""
fi])
AC_SUBST(FCFLAGS_PROFILING)
AM_CONDITIONAL([FC_USE_PROFILING],
	[test -n "$FCFLAGS_PROFILING"])
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
if test 42 = "`$SED 's/ //g' conftest.out`"; then
  $1="$5"
else
  $1="$6"
fi
if test -z "$7"; then
  rm -rf conftest* CONFTEST*
fi])

dnl
dnl --------------------------------------------------------------------
dnl
dnl WO_FC_MODULE_FILE(NAME, EXTENSION, FORTRAN_COMPILER, SOURCE_EXTENSION)
dnl
AC_DEFUN([WO_FC_MODULE_FILE],
[AC_SUBST([$1])
AC_SUBST([$2])
AC_MSG_CHECKING([for Fortran90 module file naming convention])
COMPILE_FC([wo_result], [$3], [$4],
  [module module_NAME
     implicit none
     integer, parameter, public :: forty_two = 42
   end module module_NAME], [ok], [], [KEEP])
if test -n "[$]wo_result"; then
  $1=unknown
  $2=unknown
  for name in module_name module_NAME MODULE_NAME conftest; do
    for ext in m mod M MOD d D; do
      if test -f "[$]name.[$]ext"; then
        $1="$name"
        $2="$ext"
        break 2
      fi
    done
  done
  if test X"[$]$1" = X"module_name"; then
     AC_MSG_RESULT([name: [$]$1, extension: .[$]$2 ])
  else
     AC_MSG_ERROR([unusual unsupported module file name convention: [$]$1.[$]$2])
  fi
else
  $1=""
  $2=""
  AC_MSG_RESULT([compiler failed])
  AC_MSG_NOTICE([error: **************************************************************])
  AC_MSG_NOTICE([error: Fortran compiler cannot create proper module files. This])
  AC_MSG_NOTICE([error: might be caused by linking against a wrong gcc library.])
  AC_MSG_ERROR([**************************************************************])
fi
rm -rf conftest* CONFTEST* module_name* module_NAME* MODULE_NAME*])
