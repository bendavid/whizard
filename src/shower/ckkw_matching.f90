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

module ckkw_matching

  use kinds, only: default !NODEP!
  use kinds, only: double !NODEP!
  use constants !NODEP!
  use lorentz !NODEP!
  use io_units !NODEP!
  use diagnostics !NODEP!
  use tao_random_numbers !NODEP!
  use shower_base
  use shower_partons
  use shower_core
  use ckkw_pseudo_weights

  implicit none
  private

  public :: ckkw_matching_settings_t
  public :: ckkw_matching_apply

  type :: ckkw_matching_settings_t
     real(default) :: alphaS = 0.118_default
     real(default) :: Qmin = one
     integer :: n_max_jets = 0
  end type ckkw_matching_settings_t


contains

  subroutine ckkw_matching_apply (shower, settings, weights, veto)
    type(shower_t), intent(inout) :: shower
    type(ckkw_matching_settings_t), intent(in) :: settings
    type(ckkw_pseudo_shower_weights_t), intent(in) :: weights
    logical, intent(out) :: veto

    real(default), dimension(:), allocatable :: scales
    real(double) :: weight, sf
    real(default) :: rand
    integer :: i

    if (signal_is_pending ()) return
    weight = one

    call shower%write ()

    !!! the pseudo parton shower is already simulated by shower_add_interaction
    !!! get the respective clustering scales
    allocate (scales(1:size(shower%partons)))
    do i = 1, size (shower%partons)
       if (.not. associated (shower%partons(i)%p)) cycle
       if (shower%partons(i)%p%type == 94) then
          scales(i) = 2.0 * min (parton_get_energy (shower%partons(i)%p%child1),  &
               parton_get_energy (shower%partons(i)%p%child2))**2 * &
               (1.0 - (space_part (shower%partons(i)%p%child1%momentum) * &
                space_part (shower%partons(i)%p%child2%momentum)) / &
               (space_part (shower%partons(i)%p%child1%momentum)**1 * &
                space_part (shower%partons(i)%p%child2%momentum)**1))
          scales(i) = sqrt (scales(i))
          shower%partons(i)%p%ckkwscale = scales(i)
          print *, scales(i)
       end if
    end do

    print *, " scales finished"
    !!! if (highest multiplicity) -> reweight with PDF(mu_F) / PDF(mu_cut)
    call shower%write ()

    !!! Reweight and possibly veto the whole event

    !!! calculate the relative alpha_S weight

    !! calculate the Sudakov weights for internal lines
    !! calculate the Sudakov weights for external lines
    do i = 1, size (shower%partons)
       if (signal_is_pending ()) return
       if (.not. associated (shower%partons(i)%p)) cycle
       if (shower%partons(i)%p%type == 94) then
          !!! get type
          !!! check that all particles involved are colored
          if ((parton_is_colored (shower%partons(i)%p) .or. &
               shower%partons(i)%p%ckkwtype > 0) .and. &
               (parton_is_colored (shower%partons(i)%p%child1) .or. &
               shower%partons(i)%p%child1%ckkwtype > 0) .and. &
               (parton_is_colored (shower%partons(i)%p%child1) .or. &
               shower%partons(i)%p%child1%ckkwtype > 0)) then
             print *, "reweight with alphaS(" , shower%partons(i)%p%ckkwscale, &
                  ") for particle ", shower%partons(i)%p%nr
             if (shower%partons(i)%p%belongstoFSR) then
                print *, "FSR"
                weight = weight * D_alpha_s_fsr (shower%partons(i)%p%ckkwscale**2) &
                     / settings%alphas
             else
                print *, "ISR"
                weight = weight * &
                     D_alpha_s_isr (shower%partons(i)%p%ckkwscale**2) &
                     / settings%alphas
             end if
          else
             print *, "no reweight with alphaS for ", shower%partons(i)%p%nr
          end if
          if (shower%partons(i)%p%child1%type == 94) then
             print *, "internal line from ", &
                  shower%partons(i)%p%child1%ckkwscale, &
                  " to ", shower%partons(i)%p%ckkwscale, &
                  " for type ", shower%partons(i)%p%child1%ckkwtype
             if (shower%partons(i)%p%child1%ckkwtype == 0) then
                sf = 1.0
             else if (shower%partons(i)%p%child1%ckkwtype == 1) then
                sf = SudakovQ (shower%partons(i)%p%child1%ckkwscale, &
                     shower%partons(i)%p%ckkwscale, .true.)
                print *, "SFQ = ", sf
             else if (shower%partons(i)%p%child1%ckkwtype == 2) then
                sf = SudakovG (shower%partons(i)%p%child1%ckkwscale, &
                     shower%partons(i)%p%ckkwscale, .true.)
                print *, "SFG = ", sf
             else
                print *, "SUSY not yet implemented"
             end if
             weight = weight * min (one, sf)
          else
             print *, "external line from ", settings%Qmin, &
                  shower%partons(i)%p%ckkwscale
             if (parton_is_quark (shower%partons(i)%p%child1)) then
                sf = SudakovQ (settings%Qmin, &
                     shower%partons(i)%p%ckkwscale, .true.)
                print *, "SFQ = ", sf
             else if (parton_is_gluon (shower%partons(i)%p%child1)) then
                sf = SudakovG (settings%Qmin, &
                     shower%partons(i)%p%ckkwscale, .true.)
                print *, "SFG = ", sf
             else
                print *, "not yet implemented (", &
                     shower%partons(i)%p%child2%type, ")"
                sf = one
             end if
             weight = weight * min (one, sf)
          end if
          if (shower%partons(i)%p%child2%type == 94) then
             print *, "internal line from ", shower%partons(i)%p%child2%ckkwscale, &
                  " to ", shower%partons(i)%p%ckkwscale, &
                  " for type ", shower%partons(i)%p%child2%ckkwtype
             if (shower%partons(i)%p%child2%ckkwtype == 0) then
                sf = 1.0
             else if (shower%partons(i)%p%child2%ckkwtype == 1) then
                sf = SudakovQ (shower%partons(i)%p%child2%ckkwscale, &
                     shower%partons(i)%p%ckkwscale, .true.)
                print *, "SFQ = ", sf
             else if (shower%partons(i)%p%child2%ckkwtype == 2) then
                sf = SudakovG (shower%partons(i)%p%child2%ckkwscale, &
                     shower%partons(i)%p%ckkwscale, .true.)
                print *, "SFG = ", sf
             else
                print *, "SUSY not yet implemented"
             end if
             weight = weight * min (one, sf)
          else
             print *, "external line from ", settings%Qmin, &
                  shower%partons(i)%p%ckkwscale
             if (parton_is_quark (shower%partons(i)%p%child2)) then
                sf = SudakovQ (settings%Qmin, &
                     shower%partons(i)%p%ckkwscale, .true.)
                print *, "SFQ = ", sf
             else if (parton_is_gluon (shower%partons(i)%p%child2)) then
                sf = SudakovG (settings%Qmin, &
                     shower%partons(i)%p%ckkwscale, .true.)
                print *, "SFG = ", sf
             else
                print *, "not yet implemented (", &
                     shower%partons(i)%p%child2%type, ")"
                sf = one
             end if
             weight = weight * min (one, sf)
          end if
       end if
    end do

    call tao_random_number(rand)

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

  function GammaQ (smallq, largeq, fsr) result (gamma)
    real(default), intent(in) :: smallq, largeq
    logical, intent(in) :: fsr
    real(default) :: gamma
    gamma = (8._default / three) / (pi * smallq)
    gamma = gamma * (log(largeq / smallq) - 0.75)
    if (fsr) then
       gamma = gamma * D_alpha_s_fsr (smallq**2)
    else
       gamma = gamma * D_alpha_s_isr (smallq**2)
    end if
  end function GammaQ

  function GammaG(smallq, largeq, fsr) result(gamma)
    real(default), intent(in) :: smallq, largeq
    logical, intent(in) :: fsr
    real(default) :: gamma
    gamma = 6._default / (pi * smallq)
    gamma = gamma *( log(largeq / smallq) - 11.0 / 12.0)
    if (fsr) then
       gamma = gamma * D_alpha_s_fsr(smallq**2)
    else
       gamma = gamma * D_alpha_s_isr(smallq**2)
    end if
  end function GammaG

  function GammaF (smallq, fsr) result (gamma)
    real(default), intent(in) :: smallq
    logical, intent(in) :: fsr
    real(default) :: gamma
    gamma = number_of_flavors (smallq) / (three * pi * smallq)
    if (fsr) then
       gamma = gamma * D_alpha_s_fsr (smallq**2)
    else
       gamma = gamma * D_alpha_s_isr (smallq**2)
    end if
  end function GammaF

  function SudakovQ (Q1, Q, fsr) result (sf)
    real(default), intent(in) :: Q1, Q
    logical, intent(in) :: fsr
    real(default) :: sf
    real(default) :: integral
    integer, parameter :: NTRIES = 100
    integer :: i
    real(default) :: rand
    integral = zero
    do i = 1, NTRIES
       call tao_random_number (rand)
       integral = integral + GammaQ (Q1 + rand * (Q - Q1), Q, fsr)
    end do
    integral = integral / NTRIES
    sf = exp (-integral)
  end function SudakovQ

  function SudakovG (Q1, Q, fsr) result (sf)
    real(default), intent(in) :: Q1, Q
    logical, intent(in) :: fsr
    real(default) :: sf
    real(default) :: integral
    integer, parameter :: NTRIES = 100
    integer :: i
    real(default) :: rand
    integral = zero
    do i = 1, NTRIES
       call tao_random_number (rand)
       integral = integral + GammaG (Q1 + rand * (Q - Q1), Q, fsr) + &
            GammaF (Q1 +rand * (Q - Q1), fsr)
    end do
    integral = integral / NTRIES
    sf = exp (-integral)
  end function SudakovG


end module ckkw_matching
