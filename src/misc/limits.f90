! WHIZARD 2.1.1 September 18 2012
! 
! Copyright (C) 1999-2012 by 
!     Wolfgang Kilian <kilian@physik.uni-siegen.de>
!     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
!     Juergen Reuter <juergen.reuter@desy.de>
!     Christian Speckner <christian.speckner@physik.uni-freiburg.de>
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
! This file has been stripped of most comments.  For documentation, refer
! to the source 'whizard.nw'

module limits

use iso_fortran_env, only: iostat_end, iostat_eor !NODEP!
  implicit none
  private

  integer, parameter, public :: VERSION_STRLEN = 255
  character(len=VERSION_STRLEN), parameter, public :: &
       & VERSION_STRING = "WHIZARD version 2.1.1 (September 18 2012)"
  integer, parameter, public :: MIN_UNIT = 11, MAX_UNIT = 99
  integer, parameter, public :: ENVVAR_LEN = 1000
  integer, parameter, public :: DLERROR_LEN = 160
  integer, parameter, public :: BUFFER_SIZE = 1000
  integer, parameter, public :: MAX_ERRORS = 10
  integer, parameter, public :: EOF = iostat_end,  EOR = iostat_eor
  character, parameter, public :: BLANK = ' ',  TAB = achar(9)
  character, parameter, public :: CR = achar(13), LF = achar(10)
  character, parameter, public :: BACKSLASH = achar(92)
  character(*), parameter, public :: WHITESPACE_CHARS = BLANK// TAB // CR // LF
  character(*), parameter, public :: LCLETTERS = "abcdefghijklmnopqrstuvwxyz"
  character(*), parameter, public :: UCLETTERS = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  character(*), parameter, public :: DIGITS = "0123456789"
  character(*), parameter, public :: &
           UNQUOTED = "(),|_"//LCLETTERS//UCLETTERS//DIGITS
  character(*), parameter, public :: HISTOGRAM_HEAD_FORMAT = "1x,A13,1x"
  character(*), parameter, public :: HISTOGRAM_INTG_FORMAT = "3x,I9,3x"
  character(*), parameter, public :: HISTOGRAM_DATA_FORMAT = "1PG15.8"

  integer, parameter, public :: VERTEX_TABLE_SCALE_FACTOR = 60
  double precision, parameter, public :: CIRCE1_EPSILON = 1d-6
  character(*), parameter, public :: LHAPDF_DEFAULT_PROTON = "cteq6ll.LHpdf"
  character(*), parameter, public :: LHAPDF_DEFAULT_PION   = "ABFKWPI.LHgrid"
  character(*), parameter, public :: LHAPDF_DEFAULT_PHOTON = "GSG960.LHgrid"
  character(*), parameter, public :: PDF_BUILTIN_DEFAULT_PROTON = "CTEQ6L"
  character(*), parameter, public :: PDF_BUILTIN_DEFAULT_PION   = "NONE"
  character(*), parameter, public :: PDF_BUILTIN_DEFAULT_PHOTON = "NONE"
  integer, parameter, public :: MAX_EXTERNAL = 32
  real, parameter, public :: CASCADE_SET_FILL_RATIO = 0.1
  integer, parameter, public :: MAX_WARN_RESONANCE = 50
  real, parameter, public :: GML_MIN_RANGE_RATIO = 0.02
  integer, parameter, public :: MAX_TRIES_FOR_DECAY_CHAIN = 100000
  character(*), parameter, public :: &
       RAW_EVENT_FILE_ID_STRING = "WHIZARD raw event file"
  integer, parameter, public :: ITERATIONS_DEFAULT_LIST_SIZE = 7
  integer, parameter, public :: MAX_TRIES_FOR_SINGLE_EVENT = 100000
  character(*), parameter, public :: &
       DEFAULT_ANALYSIS_FILENAME = "whizard_analysis.dat"
  character(len=1), dimension(2), parameter, public :: &
       FORBIDDEN_ENDINGS1 = (/ "o", "a" /)
  character(len=2), dimension(5), parameter, public :: &       
       FORBIDDEN_ENDINGS2 = (/ "mp", "ps", "vg", "lo", "la" /)
  character(len=3), dimension(13), parameter, public :: &
       FORBIDDEN_ENDINGS3 = (/ "aux", "dvi", "evx", "f03", "f90", "log", &
          "ltp", "mpx", "pdf", "phs", "sin", "tex", "vbg" /)
       
  integer, parameter, public :: CMDLINE_ARG_LEN = 1000

end module limits
