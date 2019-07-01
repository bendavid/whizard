! WHIZARD 2.3.0 July 21 2016
! 
! Copyright (C) 1999-2016 by 
!     Wolfgang Kilian <kilian@physik.uni-siegen.de>
!     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
!     Juergen Reuter <juergen.reuter@desy.de>
!     
!     with contributions from
!     Fabian Bach <fabian.bach@t-online.de>
!     Bijan Chokoufe <bijan.chokoufe@desy.de>
!     Christian Speckner <cnspeckn@googlemail.com> 
!     Soyoung Shim <soyoung.shim@desy.de>
!     Florian Staub <florian.staub@cern.ch>  
!     Christian Weiss <christian.weiss@desy.de>
!     and Hans-Werner Boschmann, Felix Braam, 
!     Sebastian Schmidt, So-young Shim, Daniel Wiesler 
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
  use iso_varying_string, string_t => varying_string
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
       CF = (NC**2 - one) / two / NC, &
       CA = NC, &
       TR = one / two
  real(default), public, parameter :: MZ_REF = 91.188_default
  real(default), public, parameter :: ALPHA_QCD_MZ_REF = 0.1178_default
  real(default), public, parameter :: LAMBDA_QCD_REF = 200.e-3_default
  integer, parameter, public :: UNDEFINED = 0

  integer, parameter, public :: ELECTRON = 11
  integer, parameter, public :: ELECTRON_NEUTRINO = 12
  integer, parameter, public :: MUON = 13
  integer, parameter, public :: MUON_NEUTRINO = 14
  integer, parameter, public :: TAU = 15
  integer, parameter, public :: TAU_NEUTRINO = 16

  integer, parameter, public :: GLUON = 21
  integer, parameter, public :: PHOTON = 22
  integer, parameter, public :: Z_BOSON = 23
  integer, parameter, public :: W_BOSON = 24

  integer, parameter, public :: PION = 111
  integer, parameter, public :: PIPLUS = 211
  integer, parameter, public :: PIMINUS = - PIPLUS

  integer, parameter, public :: UD0 = 2101
  integer, parameter, public :: UD1 = 2103
  integer, parameter, public :: UU1 = 2203

  integer, parameter, public :: K0L = 130
  integer, parameter, public :: K0S = 310
  integer, parameter, public :: K0 = 311
  integer, parameter, public :: KPLUS = 321
  integer, parameter, public :: DPLUS = 411
  integer, parameter, public :: D0 = 421
  integer, parameter, public :: B0 = 511
  integer, parameter, public :: BPLUS = 521

  integer, parameter, public :: PROTON = 2212
  integer, parameter, public :: NEUTRON = 2112
  integer, parameter, public :: DELTAPLUSPLUS = 2224
  integer, parameter, public :: DELTAPLUS = 2214
  integer, parameter, public :: DELTA0 = 2114
  integer, parameter, public :: DELTAMINUS = 1114

  integer, parameter, public :: SIGMAPLUS = 3222
  integer, parameter, public :: SIGMA0 = 3212
  integer, parameter, public :: SIGMAMINUS = 3112

  integer, parameter, public :: SIGMACPLUSPLUS = 4222
  integer, parameter, public :: SIGMACPLUS = 4212
  integer, parameter, public :: SIGMAC0 = 4112

  integer, parameter, public :: SIGMAB0 = 5212
  integer, parameter, public :: SIGMABPLUS = 5222

  integer, parameter, public :: BEAM_REMNANT = 9999
  integer, parameter, public :: HADRON_REMNANT = 90
  integer, parameter, public :: HADRON_REMNANT_SINGLET = 91
  integer, parameter, public :: HADRON_REMNANT_TRIPLET = 92
  integer, parameter, public :: HADRON_REMNANT_OCTET = 93

  integer, parameter, public :: INTERNAL = 94
  integer, parameter, public :: INVALID = 97

  integer, parameter, public :: COMPOSITE = 99

  integer, parameter, public:: UNKNOWN = 0
  integer, parameter, public :: SCALAR = 1, SPINOR = 2, VECTOR = 3, &
                                VECTORSPINOR = 4, TENSOR = 5

  integer, parameter, public :: BORN = 0
  integer, parameter, public :: NLO_REAL = 1
  integer, parameter, public :: NLO_VIRTUAL = 2
  integer, parameter, public :: NLO_MISMATCH = 3
  integer, parameter, public :: NLO_DGLAP = 4
  integer, parameter, public :: NLO_SUBTRACTION = 5
  integer, parameter, public :: NLO_FULL = 6
  integer, parameter, public :: GKS = 7
  integer, parameter, public :: COMPONENT_UNDEFINED = 99


  public :: component_status
  public :: is_nlo_component

  interface component_status
     module procedure component_status_of_string
     module procedure component_status_to_string
  end interface

contains

  elemental function component_status_of_string (string) result (i)
    integer :: i
    type(string_t), intent(in) :: string
    select case (char(string))
    case ("Born")
       i = BORN
    case ("Real")
       i = NLO_REAL
    case ("Virtual")
       i = NLO_VIRTUAL
    case ("Mismatch")
       i = NLO_MISMATCH
    case ("Dglap")
       i = NLO_DGLAP
    case ("Subtraction")
       i = NLO_SUBTRACTION
    case ("Full")
       i = NLO_FULL
    case ("GKS")
       i = GKS
    case default
       i = COMPONENT_UNDEFINED
    end select
  end function component_status_of_string

  elemental function component_status_to_string (i) result (string)
    type(string_t) :: string
    integer, intent(in) :: i
    select case (i)
    case (BORN)
       string = "Born"
    case (NLO_REAL)
       string = "Real"
    case (NLO_VIRTUAL)
       string = "Virtual"
    case (NLO_MISMATCH)
       string = "Mismatch"
    case (NLO_DGLAP)
       string = "Dglap"
    case (NLO_SUBTRACTION)
       string = "Subtraction"
    case (NLO_FULL)
       string = "Full"
    case (GKS)
       string = "GKS"
    case default
       string = "Undefined"
    end select
  end function component_status_to_string

  elemental function is_nlo_component (comp) result (is_nlo)
    logical :: is_nlo
    integer, intent(in) :: comp
    select case (comp)
    case (BORN : GKS)
       is_nlo = .true.
    case default
       is_nlo = .false.
    end select
  end function is_nlo_component


end module physics_defs
