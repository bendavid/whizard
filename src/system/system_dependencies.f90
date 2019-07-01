! WHIZARD <<Version>> <<Date>>
! 
! Copyright (C) 1999-2014 by 
!     Wolfgang Kilian <kilian@physik.uni-siegen.de>
!     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
!     Juergen Reuter <juergen.reuter@desy.de>
!     Christian Speckner <cnspeckn@googlemail.com>
!     with contributions by Sebastian Schmidt, Daniel Wiesler, Felix Braam
!
! WHIZARD is free software; you can redistribute it and/or modify it
! under the terms of the GNU General Public License as published by 
! the Free Software Foundation; either version 2, or (at your option)
! any later version.
!
! WHIZARD is distributed in the hope that it will be useful, but
! WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the 
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with this program; if not, write to the Free Software
! Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
module system_dependencies

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! All character strings indented by 7 blanks will be automatically
  ! split into chunks respecting the FORTRAN line length constraint by
  ! configure.
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  

  implicit none
  public
 
  ! Program version
  character(*), parameter :: WHIZARD_VERSION = "2.2.5"
  character(*), parameter :: WHIZARD_DATE = "Feb 27 2015"

  ! System paths
  ! These are used for testing without existing installation
  character(*), parameter :: WHIZARD_TEST_BASICS_MODPATH = &
       "/Users/reuter/local/packages/whizard/trunk/build/src/basics"
  character(*), parameter :: WHIZARD_TEST_UTILITIES_MODPATH = &
       "/Users/reuter/local/packages/whizard/trunk/build/src/utilities"
  character(*), parameter :: WHIZARD_TEST_TESTING_MODPATH = &
       "/Users/reuter/local/packages/whizard/trunk/build/src/testing"
  character(*), parameter :: WHIZARD_TEST_SYSTEM_MODPATH = &
       "/Users/reuter/local/packages/whizard/trunk/build/src/system"
  character(*), parameter :: WHIZARD_TEST_PHYSICS_MODPATH = &
       "/Users/reuter/local/packages/whizard/trunk/build/src/physics"
  character(*), parameter :: WHIZARD_TEST_ME_MODPATH = &
       "/Users/reuter/local/packages/whizard/trunk/build/src/matrix_elements"
  character(*), parameter :: WHIZARD_TEST_MODELS_MODPATH = &
       "/Users/reuter/local/packages/whizard/trunk/build/src/models"
  character(*), parameter :: WHIZARD_TEST_OMEGA_MODPATH = &
       "/Users/reuter/local/packages/whizard/trunk/build/omega/src"
  character(*), parameter :: WHIZARD_TEST_CORE_MODPATH = &
       "/Users/reuter/local/packages/whizard/trunk/build/src/whizard-core"
  character(*), parameter :: WHIZARD_TEST_CORE_LIBPATH = &
       "/Users/reuter/local/packages/whizard/trunk/build/src/whizard-core"
  character(*), parameter :: WHIZARD_TEST_OMEGA_BINPATH = &
       "/Users/reuter/local/packages/whizard/trunk/build/omega/bin"
  character(*), parameter :: WHIZARD_TEST_SRC_LIBPATH = &
       "/Users/reuter/local/packages/whizard/trunk/build/src"
  character(*), parameter :: WHIZARD_TEST_HEPMC_LIBPATH = &
       "/Users/reuter/local/packages/whizard/trunk/build/src/hepmc"
  character(*), parameter :: WHIZARD_TEST_LCIO_LIBPATH = &
       "/Users/reuter/local/packages/whizard/trunk/build/src/lcio"
  character(*), parameter :: WHIZARD_TEST_HOPPET_LIBPATH = &
       "/Users/reuter/local/packages/whizard/trunk/build/src/hoppet"
  character(*), parameter :: WHIZARD_TEST_MODELPATH = &
       "/Users/reuter/local/packages/whizard/trunk/share/models"
  character(*), parameter :: WHIZARD_TEST_MODELS_LIBPATH = &
       "/Users/reuter/local/packages/whizard/trunk/build/src/models"
  character(*), parameter :: WHIZARD_TEST_SUSYPATH = &
       "/Users/reuter/local/packages/whizard/trunk/share/susy"
  character(*), parameter :: WHIZARD_TEST_GMLPATH= &
       "/Users/reuter/local/packages/whizard/trunk/build/src/gamelan"
  character(*), parameter :: WHIZARD_TEST_CUTSPATH = &
       "/Users/reuter/local/packages/whizard/trunk/share/cuts"
  character(*), parameter :: WHIZARD_TEST_TESTDATAPATH = &
       "/Users/reuter/local/packages/whizard/trunk/share/test"
  character(*), parameter :: WHIZARD_TEST_TEXPATH = &
       "/Users/reuter/local/packages/whizard/trunk/src/feynmf"
  character(*), parameter :: WHIZARD_TEST_CIRCE2PATH = &
       "/Users/reuter/local/packages/whizard/trunk/circe2/share/data"
  character(*), parameter :: WHIZARD_TEST_BEAMSIMPATH = &
       "/Users/reuter/local/packages/whizard/trunk/share/beam-sim"
  character(*), parameter :: WHIZARD_TEST_MULIPATH = &
       "/Users/reuter/local/packages/whizard/trunk/share/muli"
  character(*), parameter :: PDF_BUILTIN_TEST_DATAPATH = &
       "/Users/reuter/local/packages/whizard/trunk/share/pdf_builtin"

  ! WHIZARD-specific include flags
  character(*), parameter :: WHIZARD_TEST_INCLUDES = &
      "-I" // WHIZARD_TEST_MODELS_MODPATH // " " // &
      "-I" // WHIZARD_TEST_OMEGA_MODPATH // " " // &
      "-I" // WHIZARD_TEST_CORE_MODPATH // " " // &
      "-I" // WHIZARD_TEST_ME_MODPATH // " " // &
      "-I" // WHIZARD_TEST_PHYSICS_MODPATH // " " // &
      "-I" // WHIZARD_TEST_SYSTEM_MODPATH // " " // &
      "-I" // WHIZARD_TEST_TESTING_MODPATH // " " // &
      "-I" // WHIZARD_TEST_UTILITIES_MODPATH // " " // &
      "-I" // WHIZARD_TEST_BASICS_MODPATH

  ! WHIZARD-specific link flags
  character(*), parameter :: WHIZARD_TEST_LDFLAGS = &
      "-L" // WHIZARD_TEST_CORE_LIBPATH // " " // &
      "-L" // WHIZARD_TEST_SRC_LIBPATH // " " // &
      "-L" // WHIZARD_TEST_HEPMC_LIBPATH // " " // &
      "-L" // WHIZARD_TEST_LCIO_LIBPATH // " " // &
      "-L" // WHIZARD_TEST_HOPPET_LIBPATH // " " // &
       "-lwhizard_main -lwhizard -lomega " // &
       "-lHepMC -llcio -L/usr/local//lib -lhoppet_v1"

  ! Libtool
  character(*), parameter :: WHIZARD_LIBTOOL_TEST = &
       "/Users/reuter/local/packages/whizard/trunk/build/libtool"


  ! System paths
  ! These are used for the installed version
  character(*), parameter :: PREFIX = &
       "/Users/reuter/local"
  character(*), parameter :: EXEC_PREFIX = &
       "${prefix}"
  character(*), parameter :: BINDIR = &
       "${exec_prefix}/bin"
  character(*), parameter :: LIBDIR = &
       "${exec_prefix}/lib"
  character(*), parameter :: INCLUDEDIR = &
       "${prefix}/include"
  character(*), parameter :: DATAROOTDIR = &
       "${prefix}/share"

  character(*), parameter :: PKGLIBDIR = LIBDIR  // "/whizard"
  character(*), parameter :: PKGDATADIR = DATAROOTDIR // "/whizard"
  character(*), parameter :: PKGTEXDIR = DATAROOTDIR // "/texmf/whizard"
  character(*), parameter :: PKGCIRCE2DIR = DATAROOTDIR // "/circe2"

  character(*), parameter :: WHIZARD_BASICS_MODPATH = &
       PKGLIBDIR // "/mod/basics"
  character(*), parameter :: WHIZARD_UTILITIES_MODPATH = &
       PKGLIBDIR // "/mod/utilities"
  character(*), parameter :: WHIZARD_TESTING_MODPATH = &
       PKGLIBDIR // "/mod/testing"
  character(*), parameter :: WHIZARD_SYSTEM_MODPATH = &
       PKGLIBDIR // "/mod/system"
  character(*), parameter :: WHIZARD_PHYSICS_MODPATH = &
       PKGLIBDIR // "/mod/physics"
  character(*), parameter :: WHIZARD_ME_MODPATH = &
       PKGLIBDIR // "/mod/matrix_elements"
  character(*), parameter :: WHIZARD_MODELS_MODPATH = &
       PKGLIBDIR // "/mod/models"
  character(*), parameter :: WHIZARD_OMEGA_MODPATH = &
       INCLUDEDIR // "/omega"
  character(*), parameter :: WHIZARD_CORE_MODPATH = &
       PKGLIBDIR // "/mod/whizard-core"
  character(*), parameter :: WHIZARD_OMEGA_BINPATH = &
       BINDIR
  character(*), parameter :: WHIZARD_OMEGA_LIBPATH = &
       LIBDIR
  character(*), parameter :: WHIZARD_MODELPATH = &
       PKGDATADIR // "/models"
  character(*), parameter :: WHIZARD_MODELS_LIBPATH = &
       PKGLIBDIR // "/models"
  character(*), parameter :: WHIZARD_SUSYPATH = &
       PKGDATADIR // "/susy"
  character(*), parameter :: WHIZARD_GMLPATH= &
       PKGLIBDIR // "/gamelan"
  character(*), parameter :: WHIZARD_TESTDATAPATH = &
       PKGDATADIR // "/test"
  character(*), parameter :: WHIZARD_CUTSPATH = &
       PKGDATADIR // "/cuts"
  character(*), parameter :: WHIZARD_TEXPATH = &
       PKGTEXDIR
  character(*), parameter :: WHIZARD_CIRCE2PATH = &
       PKGCIRCE2DIR // "/data"
  character(*), parameter :: WHIZARD_BEAMSIMPATH = &
       PKGDATADIR // "/beam-sim"
  character(*), parameter :: WHIZARD_MULIPATH = &
       PKGDATADIR // "/muli"
  character(*), parameter :: PDF_BUILTIN_DATAPATH = &
       PKGDATADIR // "/pdf_builtin"

  ! WHIZARD-specific include flags
  character(*), parameter :: WHIZARD_INCLUDES = &
      "-I" // WHIZARD_MODELS_MODPATH // " " // &
      "-I" // WHIZARD_OMEGA_MODPATH // " " // &
      "-I" // WHIZARD_CORE_MODPATH // " " // &
      "-I" // WHIZARD_ME_MODPATH // " " // &
      "-I" // WHIZARD_PHYSICS_MODPATH // " " // &
      "-I" // WHIZARD_SYSTEM_MODPATH // " " // &
      "-I" // WHIZARD_TESTING_MODPATH // " " // &
      "-I" // WHIZARD_UTILITIES_MODPATH // " " // &
      "-I" // WHIZARD_BASICS_MODPATH

  ! WHIZARD-specific link flags
  character(*), parameter :: WHIZARD_LDFLAGS = &
      "-L" // WHIZARD_OMEGA_LIBPATH // " " // &
       "-lwhizard_main -lwhizard -lomega " // &
       "-lHepMC -llcio -L/usr/local//lib -lhoppet_v1"

  ! Libtool
  character(*), parameter :: WHIZARD_LIBTOOL = &
      PKGLIBDIR // "/libtool"


  ! Fortran compiler
  character(*), parameter :: DEFAULT_FC = &
       "gfortran"
  character(*), parameter :: DEFAULT_FCFLAGS = &
       "  -g -O2"
  character(*), parameter :: DEFAULT_FCFLAGS_PIC = &
       " -fno-common"
  character(*), parameter :: DEFAULT_FC_SRC_EXT = &
       ".f90"

  ! Fortran compiler
  character(*), parameter :: DEFAULT_CC = &
       "gcc"
  character(*), parameter :: DEFAULT_CFLAGS = &
       "-g -O2"
  character(*), parameter :: DEFAULT_CFLAGS_PIC = &
       ""

  ! Object files
  character(*), parameter :: DEFAULT_OBJ_EXT = &
       ".o"

  ! Linker
  character(*), parameter :: DEFAULT_LD = &
       "/opt/local/bin/ld"
  character(*), parameter :: DEFAULT_LDFLAGS = &
       ""
  character(*), parameter :: DEFAULT_LDFLAGS_SO = "-shared"
  character(*), parameter :: DEFAULT_LDFLAGS_STATIC = &
       "-lstdc++"
  character(*), parameter :: DEFAULT_LDFLAGS_HEPMC = &
       "-lHepMC"
  character(*), parameter :: DEFAULT_LDFLAGS_LCIO = &
       "-llcio"
  character(*), parameter :: DEFAULT_LDFLAGS_HOPPET = &
       "-L/usr/local//lib -lhoppet_v1"
  character(*), parameter :: DEFAULT_SHLIB_EXT = ".so"

  ! Make
  character(*), parameter :: DEFAULT_MAKEFLAGS = &
       "-j1"

  ! LHAPDF library
  character(*), parameter :: LHAPDF_PDFSETS_PATH = &
       "/usr/local/share/LHAPDF"

  ! Available methods for event analysis display
  character(*), parameter :: EVENT_ANALYSIS = &
       "yes"
  character(*), parameter :: EVENT_ANALYSIS_PS = &
       "yes"
  character(*), parameter :: EVENT_ANALYSIS_PDF = &
       "yes"

  ! Programs used for event analysis display
  character(*), parameter :: PRG_LATEX  = &
       "latex"
  character(*), parameter :: PRG_MPOST  = &
       "mpost"
  character(*), parameter :: PRG_DVIPS  = &
       "dvips"
  character(*), parameter :: PRG_PS2PDF = &
       "ps2pdf14"

  ! Programs and libraries used for NLO calculations
  character(*), parameter :: GOSAM_DIR = &
       "/usr/local"
  character(*), parameter :: GOLEM_DIR = &
       "/usr/local"
  character(*), parameter :: FORM_DIR = &
       "/usr/local"
  character(*), parameter :: QGRAF_DIR = &
       "/usr/local"
  character(*), parameter :: NINJA_DIR = &
       "/usr/local"
  character(*), parameter :: SAMURAI_DIR = &
       "/usr/local"

  ! Hardwired options for batch-mode processing
  character(*), parameter :: OPT_LATEX  = &
       "-halt-on-error"
  character(*), parameter :: OPT_MPOST  = &
       "--math=scaled -halt-on-error"

  ! dlopen parameters
  integer, parameter :: &
     RTLD_LAZY   = 1 , &
     RTLD_NOW    = 2 , &
     RTLD_GLOBAL = 8 , &
     RTLD_LOCAL  = 4

  ! Misc
  logical, parameter :: LHAPDF5_AVAILABLE = .false.
  logical, parameter :: LHAPDF6_AVAILABLE = .true.

contains

  ! Subroutines that depend on configure settings

  ! OpenMP wrapper routines, work independent of OpenMP status
  function openmp_is_active () result (flag)
    logical :: flag
!    flag = .true.
    flag = .false.
  end function openmp_is_active

  subroutine openmp_set_num_threads (num)
    integer, intent(in) :: num
!    call omp_set_num_threads (num)
  end subroutine openmp_set_num_threads
  
  function openmp_get_num_threads () result (num)
    integer :: num
!    num = omp_get_num_threads ()
    num = 1
  end function openmp_get_num_threads
  
  function openmp_get_max_threads () result (num)
    integer :: num
!    num = omp_get_max_threads ()
    num = 1
  end function openmp_get_max_threads
  
  function openmp_get_default_max_threads () result (num)
    integer :: num
    num = 1
  end function openmp_get_default_max_threads

end module system_dependencies
