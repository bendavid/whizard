! WHIZARD <<Version>> <<Date>>
! 
! (C) 1999-2009 by 
!     Wolfgang Kilian <kilian@hep.physik.uni-siegen.de>
!     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
!     Juergen Reuter <juergen.reuter@physik.uni-freiburg.de>
!     with contributions by Sebastian Schmidt
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
  character(*), parameter :: WHIZARD_VERSION = "2.0.0"
  character(*), parameter :: WHIZARD_DATE = "Apr 12 2010"

  ! System paths
  ! These are used for testing without existing installation
  character(*), parameter :: WHIZARD_TEST_AUX_MODPATH = &
       "/afs/physik.uni-freiburg.de/home/reuter/Physik/progs/omwhiz/svn/trunk/build/src/misc"
  character(*), parameter :: WHIZARD_TEST_MODELS_MODPATH = &
       "/afs/physik.uni-freiburg.de/home/reuter/Physik/progs/omwhiz/svn/trunk/build/src/models"
  character(*), parameter :: WHIZARD_TEST_OMEGA_MODPATH = &
       "/afs/physik.uni-freiburg.de/home/reuter/Physik/progs/omwhiz/svn/trunk/build/src/om" // &
       "ega/src"
  character(*), parameter :: WHIZARD_TEST_CORE_MODPATH = &
       "/afs/physik.uni-freiburg.de/home/reuter/Physik/progs/omwhiz/svn/trunk/build/src/wh" // &
       "izard-core"
  character(*), parameter :: WHIZARD_TEST_CORE_LIBPATH = &
       "/afs/physik.uni-freiburg.de/home/reuter/Physik/progs/omwhiz/svn/trunk/build/src/wh" // &
       "izard-core"
  character(*), parameter :: WHIZARD_TEST_OMEGA_BINPATH = &
       "/afs/physik.uni-freiburg.de/home/reuter/Physik/progs/omwhiz/svn/trunk/build/src/om" // &
       "ega/bin"
  character(*), parameter :: WHIZARD_TEST_SRC_LIBPATH = &
       "/afs/physik.uni-freiburg.de/home/reuter/Physik/progs/omwhiz/svn/trunk/build/src"
  character(*), parameter :: WHIZARD_TEST_HEPMC_LIBPATH = &
       "/afs/physik.uni-freiburg.de/home/reuter/Physik/progs/omwhiz/svn/trunk/build/src/hepmc"
  character(*), parameter :: WHIZARD_TEST_MODELPATH = &
       "/afs/physik.uni-freiburg.de/home/reuter/Physik/progs/omwhiz/svn/trunk/share/models"
  character(*), parameter :: WHIZARD_TEST_MODELS_LIBPATH = &
       "/afs/physik.uni-freiburg.de/home/reuter/Physik/progs/omwhiz/svn/trunk/build/src/models"
  character(*), parameter :: WHIZARD_TEST_SUSYPATH = &
       "/afs/physik.uni-freiburg.de/home/reuter/Physik/progs/omwhiz/svn/trunk/share/susy"
  character(*), parameter :: WHIZARD_TEST_GMLPATH= &
       "/afs/physik.uni-freiburg.de/home/reuter/Physik/progs/omwhiz/svn/trunk/build/src/gamelan"
  character(*), parameter :: WHIZARD_TEST_CUTSPATH = &
       "/afs/physik.uni-freiburg.de/home/reuter/Physik/progs/omwhiz/svn/trunk/share/cuts"
  character(*), parameter :: WHIZARD_TEST_TESTDATAPATH = &
       "/afs/physik.uni-freiburg.de/home/reuter/Physik/progs/omwhiz/svn/trunk/share/test"
  character(*), parameter :: WHIZARD_TEST_TEXPATH = ""

  ! WHIZARD-specific include flags
  character(*), parameter :: WHIZARD_TEST_INCLUDES = &
      "-I" // WHIZARD_TEST_MODELS_MODPATH // " " // &
      "-I" // WHIZARD_TEST_OMEGA_MODPATH // " " // &
      "-I" // WHIZARD_TEST_CORE_MODPATH // " " // &
      "-I" // WHIZARD_TEST_AUX_MODPATH

  ! WHIZARD-specific link flags
  character(*), parameter :: WHIZARD_TEST_LDFLAGS = &
      "-L" // WHIZARD_TEST_CORE_LIBPATH // " " // &
      "-L" // WHIZARD_TEST_SRC_LIBPATH // " " // &
      "-L" // WHIZARD_TEST_HEPMC_LIBPATH // " " // &
       "-lwhizard_main -lwhizard -lomega -L/opt/whizard/lib -lHepMC"

  ! Libtool
  character(*), parameter :: WHIZARD_LIBTOOL_TEST = &
       "/afs/physik.uni-freiburg.de/home/reuter/Physik/progs/omwhiz/svn/trunk/build/libtool"


  ! System paths
  ! These are used for the installed version
  character(*), parameter :: PREFIX = &
       "/opt/whizard"
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

  character(*), parameter :: PKGLIBDIR = LIBDIR // "/whizard"
  character(*), parameter :: PKGDATADIR = DATAROOTDIR // "/whizard"
  character(*), parameter :: PKGTEXDIR = DATAROOTDIR // "/texmf/whizard"

  character(*), parameter :: WHIZARD_AUX_MODPATH = &
       PKGLIBDIR // "/mod/misc"
  character(*), parameter :: WHIZARD_MODELS_MODPATH = &
       PKGLIBDIR // "/mod/models"
  character(*), parameter :: WHIZARD_OMEGA_MODPATH = &
       INCLUDEDIR // "/omega"
  character(*), parameter :: WHIZARD_CORE_MODPATH = &
       PKGLIBDIR // "/mod/whizard-core"
  character(*), parameter :: WHIZARD_OMEGA_BINPATH = &
       BINDIR
  character(*), parameter :: WHIZARD_OMEGA_LIBPATH = &
       PKGLIBDIR
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

  ! WHIZARD-specific include flags
  character(*), parameter :: WHIZARD_INCLUDES = &
      "-I" // WHIZARD_MODELS_MODPATH // " " // &
      "-I" // WHIZARD_OMEGA_MODPATH // " " // &
      "-I" // WHIZARD_CORE_MODPATH // " " // &
      "-I" // WHIZARD_AUX_MODPATH

  ! WHIZARD-specific link flags
  character(*), parameter :: WHIZARD_LDFLAGS = &
      "-L" // WHIZARD_OMEGA_LIBPATH // " " // &
       "-lwhizard_main -lwhizard -lomega -L/opt/whizard/lib -lHepMC"

  ! Libtool
  character(*), parameter :: WHIZARD_LIBTOOL = &
       "/opt/whizard/lib/whizard/libtool"


  ! Fortran compiler
  character(*), parameter :: DEFAULT_FC = &
       "/opt/gcc-4.5/bin/gfortran"
  character(*), parameter :: DEFAULT_FCFLAGS = &
       " -g -O2"
  character(*), parameter :: DEFAULT_FCFLAGS_PIC = &
       " -fPIC"
  character(*), parameter :: DEFAULT_FC_SRC_EXT = &
       ".f90"
  character(*), parameter :: DEFAULT_OBJ_EXT = &
       ".o"

  ! Linker
  character(*), parameter :: DEFAULT_LD = &
       "/usr/bin/ld"
  character(*), parameter :: DEFAULT_LDFLAGS = &
       " "
  character(*), parameter :: DEFAULT_LDFLAGS_SO = "-shared"
  character(*), parameter :: DEFAULT_LDFLAGS_STATIC = &
       "-lwhizard "
  character(*), parameter :: DEFAULT_SHLIB_EXT = ".so"

  ! LHAPDF library
  character(*), parameter :: LHAPDF_PDFSETS_PATH = &
       "/opt/whizard/share/lhapdf/PDFsets"

  ! Available methods for event analysis display
  character(*), parameter :: EVENT_ANALYSIS_PS = &
       "yes"
  character(*), parameter :: EVENT_ANALYSIS_PDF = &
       "yes"

  ! Programs used for event analysis display
  character(*), parameter :: PRG_LATEX  = &
       "latex"
  character(*), parameter :: PRG_DVIPS  = &
       "dvips"
  character(*), parameter :: PRG_PS2PDF = &
       "ps2pdf14"

  ! Misc
  logical, parameter :: LHAPDF_AVAILABLE = .true.

end module system_dependencies
