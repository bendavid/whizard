! WHIZARD 2.2.6 May 02 2015
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

module ckkw_matching

  use kinds, only: default, double
  use io_units
  use constants
  use format_utils, only: write_separator
  use diagnostics
  use physics_defs
  use lorentz
  use rng_base
  use shower_base
  use shower_partons
  use ckkw_base
  use variables

  implicit none
  private

  public :: ckkw_matching_apply

contains

  subroutine ckkw_matching_apply (partons, settings, weights, rng, veto)
    type(parton_pointer_t), dimension(:), intent(inout), allocatable :: &
         partons
    type(ckkw_matching_settings_t), intent(in) :: settings
    type(ckkw_pseudo_shower_weights_t), intent(in) :: weights
    class(rng_t), intent(inout), allocatable :: rng
    logical, intent(out) :: veto

    real(default), dimension(:), allocatable :: scales
    real(double) :: weight, sf
    real(default) :: rand
    integer :: i, n_partons

    if (signal_is_pending ()) return
    weight = one

    n_partons = size (partons)

    do i = 1, n_partons
       call partons(i)%p%write ()
    end do

    !!! the pseudo parton shower is already simulated by shower_add_interaction
    !!! get the respective clustering scales
    allocate (scales (1:n_partons))
    do i = 1, n_partons
       if (.not. associated (partons(i)%p)) cycle
       if (partons(i)%p%type == INTERNAL) then
          scales(i) = two * min (partons(i)%p%child1%momentum%p(0),  &
                                 partons(i)%p%child2%momentum%p(0))**2 * &
               (1.0 - (space_part (partons(i)%p%child1%momentum) * &
                space_part (partons(i)%p%child2%momentum)) / &
               (space_part (partons(i)%p%child1%momentum)**1 * &
                space_part (partons(i)%p%child2%momentum)**1))
          scales(i) = sqrt (scales(i))
          partons(i)%p%ckkwscale = scales(i)
          print *, scales(i)
       end if
    end do

    print *, " scales finished"
    !!! if (highest multiplicity) -> reweight with PDF(mu_F) / PDF(mu_cut)
    do i = 1, n_partons
       call partons(i)%p%write ()
    end do

    !!! Reweight and possibly veto the whole event

    !!! calculate the relative alpha_S weight

    !! calculate the Sudakov weights for internal lines
    !! calculate the Sudakov weights for external lines
    do i = 1, n_partons
       if (signal_is_pending ()) return
       if (.not. associated (partons(i)%p)) cycle
       if (partons(i)%p%type == INTERNAL) then
          !!! get type
          !!! check that all particles involved are colored
          if ((partons(i)%p%is_colored () .or. &
               partons(i)%p%ckkwtype > 0) .and. &
               (partons(i)%p%child1%is_colored () .or. &
               partons(i)%p%child1%ckkwtype > 0) .and. &
               (partons(i)%p%child1%is_colored () .or. &
               partons(i)%p%child1%ckkwtype > 0)) then
             print *, "reweight with alphaS(" , partons(i)%p%ckkwscale, &
                  ") for particle ", partons(i)%p%nr
             if (partons(i)%p%belongstoFSR) then
                print *, "FSR"
                weight = weight * D_alpha_s_fsr (partons(i)%p%ckkwscale**2, &
                     partons(i)%p%settings) / settings%alphas
             else
                print *, "ISR"
                weight = weight * &
                     D_alpha_s_isr (partons(i)%p%ckkwscale**2, &
                     partons(i)%p%settings) / settings%alphas
             end if
          else
             print *, "no reweight with alphaS for ", partons(i)%p%nr
          end if
          if (partons(i)%p%child1%type == INTERNAL) then
             print *, "internal line from ", &
                  partons(i)%p%child1%ckkwscale, &
                  " to ", partons(i)%p%ckkwscale, &
                  " for type ", partons(i)%p%child1%ckkwtype
             if (partons(i)%p%child1%ckkwtype == 0) then
                sf = 1.0
             else if (partons(i)%p%child1%ckkwtype == 1) then
                sf = SudakovQ (partons(i)%p%child1%ckkwscale, &
                     partons(i)%p%ckkwscale, &
                     partons(i)%p%settings, .true., rng)
                print *, "SFQ = ", sf
             else if (partons(i)%p%child1%ckkwtype == 2) then
                sf = SudakovG (partons(i)%p%child1%ckkwscale, &
                     partons(i)%p%ckkwscale, &
                     partons(i)%p%settings, .true., rng)
                print *, "SFG = ", sf
             else
                print *, "SUSY not yet implemented"
             end if
             weight = weight * min (one, sf)
          else
             print *, "external line from ", settings%Qmin, &
                  partons(i)%p%ckkwscale
             if (partons(i)%p%child1%is_quark ()) then
                sf = SudakovQ (settings%Qmin, &
                     partons(i)%p%ckkwscale, &
                     partons(i)%p%settings, .true., rng)
                print *, "SFQ = ", sf
             else if (partons(i)%p%child1%is_gluon ()) then
                sf = SudakovG (settings%Qmin, &
                     partons(i)%p%ckkwscale, &
                     partons(i)%p%settings, .true., rng)
                print *, "SFG = ", sf
             else
                print *, "not yet implemented (", &
                     partons(i)%p%child2%type, ")"
                sf = one
             end if
             weight = weight * min (one, sf)
          end if
          if (partons(i)%p%child2%type == INTERNAL) then
             print *, "internal line from ", partons(i)%p%child2%ckkwscale, &
                  " to ", partons(i)%p%ckkwscale, &
                  " for type ", partons(i)%p%child2%ckkwtype
             if (partons(i)%p%child2%ckkwtype == 0) then
                sf = 1.0
             else if (partons(i)%p%child2%ckkwtype == 1) then
                sf = SudakovQ (partons(i)%p%child2%ckkwscale, &
                     partons(i)%p%ckkwscale, &
                     partons(i)%p%settings, .true., rng)
                print *, "SFQ = ", sf
             else if (partons(i)%p%child2%ckkwtype == 2) then
                sf = SudakovG (partons(i)%p%child2%ckkwscale, &
                     partons(i)%p%ckkwscale, &
                     partons(i)%p%settings, .true., rng)
                print *, "SFG = ", sf
             else
                print *, "SUSY not yet implemented"
             end if
             weight = weight * min (one, sf)
          else
             print *, "external line from ", settings%Qmin, &
                  partons(i)%p%ckkwscale
             if (partons(i)%p%child2%is_quark ()) then
                sf = SudakovQ (settings%Qmin, &
                     partons(i)%p%ckkwscale, &
                     partons(i)%p%settings, .true., rng)
                print *, "SFQ = ", sf
             else if (partons(i)%p%child2%is_gluon ()) then
                sf = SudakovG (settings%Qmin, &
                     partons(i)%p%ckkwscale, &
                     partons(i)%p%settings, .true., rng)
                print *, "SFG = ", sf
             else
                print *, "not yet implemented (", &
                     partons(i)%p%child2%type, ")"
                sf = one
             end if
             weight = weight * min (one, sf)
          end if
       end if
    end do

    call rng%generate (rand)

    print *, "final weight: ", weight

    !!!!!!! WRONG
    veto = .false.
    veto = (rand > weight)
    if (veto) then
       return
    end if

    !!! finally perform the parton shower
    !!! veto emissions that are too hard

    deallocate (scales)
  end subroutine ckkw_matching_apply

  function GammaQ (smallq, largeq, settings, fsr) result (gamma)
    real(default), intent(in) :: smallq, largeq
    type(shower_settings_t), intent(in) :: settings
    logical, intent(in) :: fsr
    real(default) :: gamma
    gamma = (8._default / three) / (pi * smallq)
    gamma = gamma * (log(largeq / smallq) - 0.75)
    if (fsr) then
       gamma = gamma * D_alpha_s_fsr (smallq**2, settings)
    else
       gamma = gamma * D_alpha_s_isr (smallq**2, settings)
    end if
  end function GammaQ

  function GammaG (smallq, largeq, settings, fsr) result (gamma)
    real(default), intent(in) :: smallq, largeq
    type(shower_settings_t), intent(in) :: settings
    logical, intent(in) :: fsr
    real(default) :: gamma
    gamma = 6._default / (pi * smallq)
    gamma = gamma *( log(largeq / smallq) - 11.0 / 12.0)
    if (fsr) then
       gamma = gamma * D_alpha_s_fsr (smallq**2, settings)
    else
       gamma = gamma * D_alpha_s_isr (smallq**2, settings)
    end if
  end function GammaG

  function GammaF (smallq, settings, fsr) result (gamma)
    real(default), intent(in) :: smallq
    type(shower_settings_t), intent(in) :: settings
    logical, intent(in) :: fsr
    real(default) :: gamma
    gamma = number_of_flavors (smallq, settings%max_n_flavors, &
         settings%d_min_t) / (three * pi * smallq)
    if (fsr) then
       gamma = gamma * D_alpha_s_fsr (smallq**2, settings)
    else
       gamma = gamma * D_alpha_s_isr (smallq**2, settings)
    end if
  end function GammaF

  function SudakovQ (Q1, Q, settings, fsr, rng) result (sf)
    real(default), intent(in) :: Q1, Q
    type(shower_settings_t), intent(in) :: settings
    class(rng_t), intent(inout), allocatable :: rng
    logical, intent(in) :: fsr
    real(default) :: sf
    real(default) :: integral
    integer, parameter :: NTRIES = 100
    integer :: i
    real(default) :: rand
    integral = zero
    do i = 1, NTRIES
       call rng%generate (rand)
       integral = integral + GammaQ (Q1 + rand * (Q - Q1), Q, settings, fsr)
    end do
    integral = integral / NTRIES
    sf = exp (-integral)
  end function SudakovQ

  function SudakovG (Q1, Q, settings, fsr, rng) result (sf)
    real(default), intent(in) :: Q1, Q
    type(shower_settings_t), intent(in) :: settings
    logical, intent(in) :: fsr
    real(default) :: sf
    real(default) :: integral
    class(rng_t), intent(inout), allocatable :: rng
    integer, parameter :: NTRIES = 100
    integer :: i
    real(default) :: rand
    integral = zero
    do i = 1, NTRIES
       call rng%generate (rand)
       integral = integral + &
            GammaG (Q1 + rand * (Q - Q1), Q, settings, fsr) + &
            GammaF (Q1 + rand * (Q - Q1), settings, fsr)
    end do
    integral = integral / NTRIES
    sf = exp (-integral)
  end function SudakovG


end module ckkw_matching
