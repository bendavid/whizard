! WHIZARD 2.2.4 Feb 06 2015
! 
! Copyright (C) 1999-2015 by 
!     Wolfgang Kilian <kilian@physik.uni-siegen.de>
!     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
!     Juergen Reuter <juergen.reuter@desy.de>
!     
!     with contributions from
!     Fabian Bach <fabian.bach@desy.de>
!     Christian Speckner <cnspeckn@googlemail.com> 
!     Christian Weiss <christian.weiss@desy.de>
!     and Hans-Werner Boschmann, Felix Braam, 
!     Sebastian Schmidt, Daniel Wiesler 
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

module shower_base

  use kinds, only: default
  use constants
  use diagnostics
  use rng_base
  use physics_defs
  use sm_physics, only: running_as_lam

  implicit none
  private

  public :: D_alpha_s_isr
  public :: D_alpha_s_fsr
  public :: mass_type
  public :: mass_squared_type
  public :: number_of_flavors
  public :: shower_set_D_min_t
  public :: shower_set_D_lambda_fsr
  public :: shower_set_D_lambda_isr
  public :: shower_set_D_Nf
  public :: shower_set_D_running_alpha_s_fsr
  public :: shower_set_D_running_alpha_s_isr
  public :: shower_set_D_constantalpha_s
  public :: shower_set_isr_pt_ordered
  public :: shower_set_primordial_kt_width
  public :: shower_set_primordial_kt_cutoff
  public :: shower_set_rng
  public :: shower_get_rng

  integer, parameter, public :: STRF_NONE = 0
  integer, parameter, public :: STRF_LHAPDF6 = 1
  integer, parameter, public :: STRF_LHAPDF5 = 2
  integer, parameter, public :: STRF_PDF_BUILTIN = 3

  logical, parameter, public :: D_PRINT = .false.
  logical, parameter, public :: ASSERT = .false.
  real(default), public :: D_Min_t = one
  real(default), public :: D_min_scale = 0.5_default
  real(default), public :: D_Lambda_fsr = 0.29_default
  real(default), public :: D_Lambda_isr = 0.29_default
  integer, public :: D_Nf = 5
  logical, public :: D_running_alpha_s_fsr = .true.
  logical, public :: D_running_alpha_s_isr = .true.
  real(default), public :: D_constalpha_s = 0.20_default
  logical, public :: alpha_s_fudged = .true.
  logical, public :: isr_pt_ordered = .false.
  logical, public :: treat_light_quarks_massless = .true.
  logical, public :: treat_duscb_quarks_massless = .false.
  real(default), public :: primordial_kt_width = 1.5_default
  real(default), public :: primordial_kt_cutoff = five
  real(default), public :: scalefactor1 = 0.02_default
  real(default), public :: scalefactor2 = 0.02_default
    class(rng_t), allocatable, public :: rng

contains

  function D_alpha_s_isr (tin) result (alpha_s)
    real(default), intent(in) :: tin
    real(default) :: t
    real(default) :: alpha_s
    if (alpha_s_fudged) then
       t = max (max (0.1_default * D_Min_t, &
                     1.1_default * D_Lambda_isr**2), abs(tin))
    else
       t = abs(tin)
    end if
    if (D_running_alpha_s_isr) then
       alpha_s = running_as_lam (number_of_flavors(t), sqrt(t), D_Lambda_isr, 0)
    else
       alpha_s = D_constalpha_s
    end if
  end function D_alpha_s_isr

  function D_alpha_s_fsr (tin) result (alpha_s)
    real(default), intent(in) :: tin
    real(default) :: t
    real(default) :: alpha_s
    if (alpha_s_fudged) then
       t = max (max (0.1_default * D_Min_t, &
                     1.1_default * D_Lambda_isr**2), abs(tin))
    else
       t = abs(tin)
    end if
    if (D_running_alpha_s_fsr) then
       alpha_s = running_as_lam (number_of_flavors(t), sqrt(t), D_Lambda_fsr, 0)
    else
       alpha_s = D_constalpha_s
    end if
  end function D_alpha_s_fsr

  elemental function mass_type (type) result (mass)
    integer, intent(in) :: type
    real(default) :: mass
    mass = sqrt (mass_squared_type (type))
  end function mass_type

  elemental function mass_squared_type (type) result (mass2)
    integer, intent(in) :: type
    real(default) :: mass2

    select case (abs (type))
    case (1,2)
       if (treat_light_quarks_massless .or. &
            treat_duscb_quarks_massless) then
          mass2 = zero
       else
          mass2 = 0.330_default**2
       end if
    case (3)
       if (treat_duscb_quarks_massless) then
          mass2 = zero
       else
          mass2 = 0.500_default**2
       end if
    case (4)
       if (treat_duscb_quarks_massless) then
          mass2 = zero
       else
          mass2 = 1.500_default**2
       end if
    case (5)
       if (treat_duscb_quarks_massless) then
          mass2 = zero
       else
          mass2 = 4.800_default**2
       end if
    case (6)
       mass2 = 175.00_default**2
    case (GLUON)
       mass2 = zero
    case (NEUTRON)
       mass2 = 0.939565_default**2
    case (PROTON)
       mass2 = 0.93827_default**2
    case (DPLUS)
       mass2 = 1.86960_default**2
    case (D0)
       mass2 = 1.86483_default**2
    case (B0)
       mass2 = 5.27950_default**2
    case (BPLUS)
       mass2 = 5.27917_default**2
    case (DELTAPLUSPLUS)
       mass2 = 1.232_default**2
    case (SIGMA0)
       mass2 = 1.192642_default**2
    case (SIGMAPLUS)
       mass2 = 1.18937_default**2
    case (SIGMACPLUS)
       mass2 = 2.4529_default**2
    case (SIGMACPLUSPLUS)
       mass2 = 2.45402_default**2
    case (SIGMAB0)
       mass2 = 5.8152_default**2
    case (SIGMABPLUS)
       mass2 = 5.8078_default**2
    case (UNDEFINED)
       mass2 = zero
    case (BEAM_REMNANT)
       mass2 = zero ! don't know how to handle the beamremnant
    case default !others not implemented
       mass2 = zero
    end select
  end function mass_squared_type

  elemental function number_of_flavors (t) result (nr)
    real(default), intent(in) :: t
    real(default) :: nr
    integer :: i
    nr = 0
    if (t < D_Min_t) return   ! arbitrary cut off
    ! TODO: do i = 1, min (max (3, D_Nf), 6)
    do i = 1, min (3, D_Nf)
    !!! to do: take heavier quarks(-> cuts on allowed costheta in g->qq)
    !!!        into account
       if ((four * mass_squared_type (i) + D_Min_t) < t ) then
          nr = i
       else
          exit
       end if
    end do
  end function number_of_flavors

  subroutine shower_set_D_Min_t (input)
    real(default) :: input
    D_Min_t = input
  end subroutine shower_set_D_Min_t

  subroutine shower_set_D_Lambda_fsr (input)
    real(default) :: input
    D_Lambda_fsr = input
  end subroutine shower_set_D_Lambda_fsr

  subroutine shower_set_D_Lambda_isr (input)
    real(default) :: input
    D_Lambda_isr = input
  end subroutine shower_set_D_Lambda_isr

  subroutine shower_set_D_Nf (input)
    integer :: input
    D_Nf = input
  end subroutine shower_set_D_Nf

  subroutine shower_set_D_running_alpha_s_fsr (input)
    logical :: input
    D_running_alpha_s_fsr = input
  end subroutine shower_set_D_running_alpha_s_fsr

  subroutine shower_set_D_running_alpha_s_isr (input)
    logical :: input
    D_running_alpha_s_isr = input
  end subroutine shower_set_D_running_alpha_s_isr

  subroutine shower_set_D_constantalpha_s (input)
    real(default) :: input
    D_constalpha_s = input
  end subroutine shower_set_D_constantalpha_s

  subroutine shower_set_isr_pt_ordered (input)
    logical :: input
    isr_pt_ordered = input
  end subroutine shower_set_isr_pt_ordered

  subroutine shower_set_primordial_kt_width (input)
    real(default) :: input
    primordial_kt_width = input
  end subroutine shower_set_primordial_kt_width

  subroutine shower_set_primordial_kt_cutoff (input)
    real(default) :: input
    primordial_kt_cutoff = input
  end subroutine shower_set_primordial_kt_cutoff

  subroutine shower_set_rng (rng_inc)
    class(rng_t), intent(inout), allocatable :: rng_inc
    call move_alloc (from = rng_inc, to = rng)
  end subroutine shower_set_rng

  subroutine shower_get_rng (rng_out)
    class(rng_t), intent(inout), allocatable :: rng_out
    call move_alloc (from = rng, to = rng_out)
  end subroutine shower_get_rng


end module shower_base
