! WHIZARD 2.2.3 Nov 30 2014
! 
! Copyright (C) 1999-2014 by 
!     Wolfgang Kilian <kilian@physik.uni-siegen.de>
!     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
!     Juergen Reuter <juergen.reuter@desy.de>
!     
!     with contributions from
!     Fabian Bach <fabian.bach@desy.de>
!     Christian Speckner <cnspeckn@googlemail.com> 
!     Christian Weiss <christian.weiss@desy.de>
!     and Felix Braam, Sebastian Schmidt, Daniel Wiesler 
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

module physics_defs

  use kinds, only: default
  use constants, only: one, two, three

  implicit none
  private

  real(default), parameter, public :: &
       conv = 0.38937966e12_default
  real(default), parameter, public :: &
       pb_per_fb = 1.e-3_default
  character(*), parameter, public :: &
       energy_unit = "GeV"
  character(*), parameter, public :: &
       cross_section_unit = "fb"
  real(default), parameter, public :: &
       NC = three, &
       CF = (NC**2 - one)/two/NC, &
       CA = NC, &
       TR = one/two
  real(default), public, parameter :: MZ_REF = 91.188_default
  real(default), public, parameter :: ALPHA_QCD_MZ_REF = 0.1178_default
  real(default), public, parameter :: LAMBDA_QCD_REF = 200.e-3_default
  integer, parameter, public :: UNDEFINED = 0
  integer, parameter, public :: ELECTRON = 11

  integer, parameter, public :: GLUON = 21
  integer, parameter, public :: PHOTON = 22
  integer, parameter, public :: Z_BOSON = 23
  integer, parameter, public :: W_BOSON = 24

  integer, parameter, public :: PROTON = 2212 
  integer, parameter, public :: PION = 111
  integer, parameter, public :: PIPLUS = 211
  integer, parameter, public :: PIMINUS = - PIPLUS

  integer, parameter, public :: HADRON_REMNANT = 90
  integer, parameter, public :: HADRON_REMNANT_SINGLET = 91
  integer, parameter, public :: HADRON_REMNANT_TRIPLET = 92
  integer, parameter, public :: HADRON_REMNANT_OCTET = 93

  integer, parameter, public :: PRT_ANY = 81
  integer, parameter, public :: PRT_VISIBLE = 82
  integer, parameter, public :: PRT_CHARGED = 83
  integer, parameter, public :: PRT_COLORED = 84

  integer, parameter, public :: INVALID = 97
  integer, parameter, public :: KEYSTONE = 98
  integer, parameter, public :: COMPOSITE = 99

  integer, parameter, public:: UNKNOWN=0
  integer, parameter, public :: SCALAR=1, SPINOR=2, VECTOR=3, &
        VECTORSPINOR=4, TENSOR=5

end module physics_defs
