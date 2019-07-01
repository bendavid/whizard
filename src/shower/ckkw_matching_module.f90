!!! module: ckkw_matching_module
!!! This code is part of my Ph.D studies.
!!! 
!!! Copyright (C) 2012 Sebastian Schmidt <sebastian.t.schmidt@desy.de>
!!! 
!!! This program is free software; you can redistribute it and/or modify it
!!! under the terms of the GNU General Public License as published by the Free 
!!! Software Foundation; either version 3 of the License, or (at your option) 
!!! any later version.
!!! 
!!! This program is distributed in the hope that it will be useful, but WITHOUT
!!! ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or 
!!! FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
!!! more details.
!!! 
!!! You should have received a copy of the GNU General Public License along
!!! with this program; if not, see <http://www.gnu.org/licenses/>.
!!! 
!!! Latest Change: Thu Jul 19 18:31:15 2012 Time zone: 7200 seconds
!!! 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
module ckkw_matching_module
  use kinds, only: default, double !NODEP!
  use lorentz !NODEP!
  use file_utils !NODEP!
  use tao_random_numbers !NODEP!
  use shower_basics_module
  use shower_parton_module
  use shower_module
  use ckkw_pseudo_weights_module

  implicit none

  type :: ckkw_matching_settings_t
     real(kind=default) :: alphaS
     real(kind=default) :: Qmin = 1.0_default
     integer :: n_max_jets = 0
  end type ckkw_matching_settings_t

contains

  subroutine ckkw_matching(shower, ckkw_matching_settings, ckkw_matching_weights, veto)
    type(shower_t), intent(inout) :: shower
    type(ckkw_matching_settings_t), intent(in) :: ckkw_matching_settings
    type(ckkw_pseudo_shower_weights_t), intent(in) :: ckkw_matching_weights
    logical, intent(out) :: veto

    real(kind=default), dimension(:), allocatable :: scales
    real(kind=double) :: weight, sf
    real(kind=default) :: rand 
    integer :: i

    weight = 1.0

    call shower_print(shower)

    ! the pseudo parton shower is already simulated by shower_add_interaction
    
    ! get the respective clustering scales
    allocate(scales(1:size(shower%partons)))
    do i=1, size(shower%partons)
       if(.not.associated(shower%partons(i)%p)) cycle
       if(shower%partons(i)%p%typ .eq. 94) then
          scales(i) = 2.0*min( parton_get_energy(shower%partons(i)%p%child1),  parton_get_energy(shower%partons(i)%p%child2))**2 * &
               (1.0 - ( space_part(shower%partons(i)%p%child1%momentum) * space_part(shower%partons(i)%p%child2%momentum)) / &
               (space_part(shower%partons(i)%p%child1%momentum)**1 * space_part(shower%partons(i)%p%child2%momentum)**1) )
          scales(i) = sqrt(scales(i))
          shower%partons(i)%p%ckkwscale = scales(i)
          print *, scales(i)
       end if      
    end do

    print *, " scales finished"
    ! if(highest multiplicity) -> reweight with PDF(mu_F) / PDF(mu_cut)
    call shower_print(shower)

    ! Reweight and possibly veto the whole event

    !! calculate the relative alpha_S weight

    !! calculate the Sudakov weights for internal lines
    !! calculate the Sudakov weights for external lines
    do i=1, size(shower%partons)
       if(.not.associated(shower%partons(i)%p)) cycle
       if(shower%partons(i)%p%typ .eq. 94) then
          ! get type
          ! check that all particles involved are colored
          if( (parton_is_colored(shower%partons(i)%p) .or. shower%partons(i)%p%ckkwtype.gt.0) .and. &
               (parton_is_colored(shower%partons(i)%p%child1) .or. shower%partons(i)%p%child1%ckkwtype.gt.0) .and. &
               (parton_is_colored(shower%partons(i)%p%child1) .or. shower%partons(i)%p%child1%ckkwtype.gt.0) ) then
             print *, "reweight with alphaS(" , shower%partons(i)%p%ckkwscale, ") for particle ", shower%partons(i)%p%nr
             if(shower%partons(i)%p%belongstoFSR) then
                print *, "FSR"
                weight = weight * D_alpha_s_fsr(shower%partons(i)%p%ckkwscale**2) / ckkw_matching_settings%alphas
             else
                print *, "ISR"
                weight = weight * D_alpha_s_isr(shower%partons(i)%p%ckkwscale**2) / ckkw_matching_settings%alphas
             end if
          else 
             print *, "no reweight with alphaS for ", shower%partons(i)%p%nr
          end if
          if(shower%partons(i)%p%child1%typ .eq. 94) then
             print *, "internal line from ", shower%partons(i)%p%child1%ckkwscale, " to ", shower%partons(i)%p%ckkwscale, &
                  " for type ", shower%partons(i)%p%child1%ckkwtype
             if(shower%partons(i)%p%child1%ckkwtype == 0) then
                sf = 1.0
             else if(shower%partons(i)%p%child1%ckkwtype == 1) then
                sf = SudakovQ(shower%partons(i)%p%child1%ckkwscale, shower%partons(i)%p%ckkwscale, .true.)
                print *, "SFQ = ", sf
             else if(shower%partons(i)%p%child1%ckkwtype == 2) then
                sf = SudakovG(shower%partons(i)%p%child1%ckkwscale, shower%partons(i)%p%ckkwscale, .true.)
                print *, "SFG = ", sf
             else
                print *, "SUSY not yet implemented"
             end if
             weight = weight*min(1.0_default,sf)
          else
             print *, "external line from ", ckkw_matching_settings%Qmin, shower%partons(i)%p%ckkwscale
             if(parton_is_quark(shower%partons(i)%p%child1)) then
                sf = SudakovQ(ckkw_matching_settings%Qmin, shower%partons(i)%p%ckkwscale, .true.)
                print *, "SFQ = ", sf
             else if(parton_is_gluon(shower%partons(i)%p%child1)) then
                sf = SudakovG(ckkw_matching_settings%Qmin, shower%partons(i)%p%ckkwscale, .true.)
                print *, "SFG = ", sf
             else 
                print *, "not yet implemented (", shower%partons(i)%p%child2%typ, ")"
                sf = 1.0_default
             end if
             weight = weight*min(1.0_default,sf)
          end if
          if(shower%partons(i)%p%child2%typ .eq. 94) then
             print *, "internal line from ", shower%partons(i)%p%child2%ckkwscale, " to ", shower%partons(i)%p%ckkwscale, &
                  " for type ", shower%partons(i)%p%child2%ckkwtype
             if(shower%partons(i)%p%child2%ckkwtype == 0) then
                sf = 1.0
             else if(shower%partons(i)%p%child2%ckkwtype == 1) then
                sf = SudakovQ(shower%partons(i)%p%child2%ckkwscale, shower%partons(i)%p%ckkwscale, .true.)
                print *, "SFQ = ", sf
             else if(shower%partons(i)%p%child2%ckkwtype == 2) then
                sf = SudakovG(shower%partons(i)%p%child2%ckkwscale, shower%partons(i)%p%ckkwscale, .true.)
                print *, "SFG = ", sf
             else
                print *, "SUSY not yet implemented"
             end if
             weight = weight*min(1.0_default,sf)
          else
             print *, "external line from ", ckkw_matching_settings%Qmin, shower%partons(i)%p%ckkwscale
             if(parton_is_quark(shower%partons(i)%p%child2)) then
                sf = SudakovQ(ckkw_matching_settings%Qmin, shower%partons(i)%p%ckkwscale, .true.)
                print *, "SFQ = ", sf
             else if(parton_is_gluon(shower%partons(i)%p%child2)) then
                sf = SudakovG(ckkw_matching_settings%Qmin, shower%partons(i)%p%ckkwscale, .true.)
                print *, "SFG = ", sf
             else 
                print *, "not yet implemented (", shower%partons(i)%p%child2%typ, ")"
                sf = 1.0_default
             end if
             weight = weight*min(1.0_default,sf)
          end if
       end if
    end do

    call tao_random_number(rand)

    print *, "final weight: ", weight

    !!!!!!! WRONG
    veto = .false.
    veto =(rand > weight)
    if(veto) then
       return
    end if

    ! finally perform the parton shower
    ! veto emissions that are too hard

    deallocate(scales)
  end subroutine ckkw_matching

  function GammaQ(smallq, largeq, fsr) result(gamma)
    real(kind=default), intent(in) :: smallq, largeq
    logical, intent(in) :: fsr
    real(kind=default) :: gamma

    gamma = (2.0*4.0/3.0)/(pi*smallq)
    gamma = gamma *( log(largeq/smallq) - 0.75)
    if(fsr) then
       gamma = gamma * D_alpha_s_fsr(smallq**2)
    else
       gamma = gamma * D_alpha_s_isr(smallq**2)
    end if
  end function GammaQ

  function GammaG(smallq, largeq, fsr) result(gamma)
    real(kind=default), intent(in) :: smallq, largeq
    logical, intent(in) :: fsr
    real(kind=default) :: gamma

    gamma = (2.0*3.0)/(pi*smallq)
    gamma = gamma *( log(largeq/smallq) - 11.0/12.0)
    if(fsr) then
       gamma = gamma * D_alpha_s_fsr(smallq**2)
    else
       gamma = gamma * D_alpha_s_isr(smallq**2)
    end if
  end function GammaG

  function GammaF(smallq, fsr) result(gamma)
    real(kind=default), intent(in) :: smallq
    logical, intent(in) :: fsr
    real(kind=default) :: gamma

    gamma = number_of_flavors(smallq)/(3.0*pi*smallq)
    if(fsr) then
       gamma = gamma * D_alpha_s_fsr(smallq**2)
    else
       gamma = gamma * D_alpha_s_isr(smallq**2)
    end if
  end function GammaF

  function SudakovQ(Q1, Q, fsr) result(sf)
    real(kind=default), intent(in) :: Q1, Q
    logical, intent(in) :: fsr
    real(kind=default) :: sf

    real(kind=default) :: integral
    integer, parameter :: NTRIES = 100
    integer :: i
    real(kind=default) :: rand

    integral = 0.0;

    do i=1, NTRIES
       call tao_random_number(rand)
       integral = integral + GammaQ( Q1 + rand*(Q-Q1), Q, fsr)
    end do
    integral = integral * (1.0/NTRIES)
    
    sf = exp(-integral)
  end function SudakovQ

  function SudakovG(Q1, Q, fsr) result(sf)
    real(kind=default), intent(in) :: Q1, Q
    logical, intent(in) :: fsr
    real(kind=default) :: sf

    real(kind=default) :: integral
    integer, parameter :: NTRIES = 100
    integer :: i
    real(kind=default) :: rand

    integral = 0.0;

    do i=1, NTRIES
       call tao_random_number(rand)
       integral = integral + GammaG( Q1 + rand*(Q-Q1), Q, fsr) + &
            GammaF(Q1 +rand*(Q-Q1), fsr)
    end do
    integral = integral * (1.0/NTRIES)
    
    sf = exp(-integral)
  end function SudakovG

end module ckkw_matching_module
