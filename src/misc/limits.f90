! WHIZARD 2.2.1 June 3 2014
! 
! Copyright (C) 1999-2014 by 
!     Wolfgang Kilian <kilian@physik.uni-siegen.de>
!     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
!     Juergen Reuter <juergen.reuter@desy.de>
!     
!     with contributions from
!     Christian Speckner <cnspeckn@googlemail.com> 
!     and  Fabian Bach, Felix Braam, Sebastian Schmidt, Daniel Wiesler 
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
  use kinds, only: default !NODEP!

  implicit none
  private

  integer, parameter, public :: VERSION_STRLEN = 255
  character(len=VERSION_STRLEN), parameter, public :: &
       & VERSION_STRING = "WHIZARD version 2.2.1 (June 3 2014)"
  character(*), parameter, public :: FMT_19 = "ES19.12"
  character(*), parameter, public :: FMT_18 = "ES18.11"
  character(*), parameter, public :: FMT_17 = "ES17.10"  
  character(*), parameter, public :: FMT_16 = "ES16.9"
  character(*), parameter, public :: FMT_15 = "ES15.8"
  character(*), parameter, public :: FMT_14 = "ES14.7"    
  character(*), parameter, public :: FMT_13 = "ES13.6"    
  character(*), parameter, public :: FMT_12 = "ES12.5"
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
  real(default), public, parameter :: MZ_REF = 91.188_default
  real(default), public, parameter :: ALPHA_QCD_MZ_REF = 0.1178_default
  real(default), public, parameter :: LAMBDA_QCD_REF = 200.e-3_default
  character(*), parameter, public :: HISTOGRAM_HEAD_FORMAT = "1x,A15,3x"
  character(*), parameter, public :: HISTOGRAM_INTG_FORMAT = "3x,I9,3x"
  character(*), parameter, public :: HISTOGRAM_DATA_FORMAT = FMT_19

  integer, parameter, public :: VERTEX_TABLE_SCALE_FACTOR = 60
  character(*), parameter, public :: LHAPDF_DEFAULT_PROTON = "cteq6ll.LHpdf"
  character(*), parameter, public :: LHAPDF_DEFAULT_PION   = "ABFKWPI.LHgrid"
  character(*), parameter, public :: LHAPDF_DEFAULT_PHOTON = "GSG960.LHgrid"
  character(*), parameter, public :: PDF_BUILTIN_DEFAULT_PROTON = "CTEQ6L"
  character(*), parameter, public :: PDF_BUILTIN_DEFAULT_PION   = "NONE"
  character(*), parameter, public :: PDF_BUILTIN_DEFAULT_PHOTON = "MRST2004QEDp"
  integer, parameter, public :: MAX_EXTERNAL = 32
  real, parameter, public :: CASCADE_SET_FILL_RATIO = 0.1
  integer, parameter, public :: MAX_WARN_RESONANCE = 50
  real(default), parameter, public :: INTEGRATION_ERROR_TOLERANCE = 1e-10
  real, parameter, public :: GML_MIN_RANGE_RATIO = 0.02
  integer, parameter, public :: SHOW_BUFFER_SIZE = 4096
  character(*), parameter, public :: &
       DEFAULT_ANALYSIS_FILENAME = "whizard_analysis.dat"
  character(len=1), dimension(2), parameter, public :: &
       FORBIDDEN_ENDINGS1 = [ "o", "a" ]
  character(len=2), dimension(5), parameter, public :: &       
       FORBIDDEN_ENDINGS2 = [ "mp", "ps", "vg", "lo", "la" ]
  character(len=3), dimension(14), parameter, public :: &
       FORBIDDEN_ENDINGS3 = [ "aux", "dvi", "evt", "evx", "f03", "f90", &
          "f95", "log", "ltp", "mpx", "pdf", "phs", "sin", "tex" ]
       
  integer, parameter, public :: CMDLINE_ARG_LEN = 1000

end module limits
