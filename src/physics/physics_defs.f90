! WHIZARD 2.2.8 Nov 22 2015
! 
! Copyright (C) 1999-2015 by 
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
  integer, parameter, public :: NLO_PDF = 3
  integer, parameter, public :: NLO_SUBTRACTION = 4
  integer, parameter, public :: GKS = 5
  integer, parameter, public :: NLO_THRESHOLD_RESUMMATION = 6


end module physics_defs
