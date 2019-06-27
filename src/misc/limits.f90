! WHIZARD 2.0.1 Sun Apr 25 2010
! 
! (C) 1999-2010 by 
!     Wolfgang Kilian <kilian@hep.physik.uni-siegen.de>
!     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
!     Juergen Reuter <juergen.reuter@physik.uni-freiburg.de>
!     with contributions by Christian Speckner, Sebastian Schmidt, 
!     Daniel Wiesler, Felix Braam
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
       & VERSION_STRING = "WHIZARD version 2.0.1 (Sun Apr 25 2010)"
  integer, parameter, public :: MIN_UNIT = 11, MAX_UNIT = 99
  integer, parameter, public :: FILENAME_LEN = 256
  character(len=*), parameter, public :: DEFAULT_FILENAME = "whizard"
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
  character(*), parameter, public :: LHAPDF_DEFAULT_PROTON = "cteq6ll.LHpdf"
  character(*), parameter, public :: LHAPDF_DEFAULT_PION   = "ABFKWPI.LHgrid"
  character(*), parameter, public :: LHAPDF_DEFAULT_PHOTON = "GSG960.LHgrid"
  integer, parameter, public :: MAX_EXTERNAL = 32
  real, parameter, public :: CASCADE_SET_FILL_RATIO = 0.1
  integer, parameter, public :: MAX_WARN_RESONANCE = 50
  integer, parameter, public :: MAX_TRIES_FOR_DECAY_CHAIN = 100000
  integer, parameter, public :: RAW_EVENT_FILE_VERSION = 1
  integer, parameter, public :: ITERATIONS_DEFAULT_LIST_SIZE = 7
  integer, parameter, public :: CMDLINE_ARG_LEN = 1000

end module limits
