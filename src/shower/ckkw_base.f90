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

module ckkw_base

  use kinds, only: default
  use io_units
  use constants
  use format_utils, only: write_separator
  use shower_base
  use variables

  implicit none
  private

  public :: ckkw_matching_settings_t
  public :: ckkw_pseudo_shower_weights_t
  public :: ckkw_pseudo_shower_weights_init
  public :: ckkw_pseudo_shower_weights_write
  public :: ckkw_matching_data_t

  type, extends (matching_settings_t) :: ckkw_matching_settings_t
     real(default) :: alphaS = 0.118_default
     real(default) :: Qmin = one
     integer :: n_max_jets = 0
   contains
     procedure :: init => ckkw_matching_settings_init
     procedure :: write => ckkw_matching_settings_write
  end type ckkw_matching_settings_t

  type :: ckkw_pseudo_shower_weights_t
     real(default) :: alphaS
     real(default), dimension(:), allocatable :: weights
     real(default), dimension(:,:), allocatable :: weights_by_type
  end type ckkw_pseudo_shower_weights_t

  type, extends (matching_data_t) :: ckkw_matching_data_t
     type(ckkw_pseudo_shower_weights_t) :: ckkw_weights
  end type ckkw_matching_data_t


contains

  subroutine ckkw_matching_settings_init (settings, var_list)
    class(ckkw_matching_settings_t), intent(out) :: settings
    type(var_list_t), intent(in) :: var_list
  end subroutine ckkw_matching_settings_init

  subroutine ckkw_matching_settings_write (settings, unit)
    class(ckkw_matching_settings_t), intent(in) :: settings
    integer, intent(in), optional :: unit
    integer :: u
    u = given_output_unit (unit);  if (u < 0)  return
    write (u, "(1x,A)")  "CKKW matching settings:"
    call write_separator (u)
    write (u, "(3x,A,1x,ES19.12)") &
         "alphaS       = ", settings%alphaS
    write (u, "(3x,A,1x,ES19.12)") &
         "Qmin         = ", settings%Qmin
    write (u, "(3x,A,1x,I0)") &
         "n_max_jets   = ", settings%n_max_jets
  end subroutine ckkw_matching_settings_write

  subroutine ckkw_pseudo_shower_weights_init (weights)
    type(ckkw_pseudo_shower_weights_t), intent(out) :: weights
    weights%alphaS = zero
    if (allocated (weights%weights))  deallocate(weights%weights)
    if (allocated (weights%weights_by_type))  &
         deallocate(weights%weights_by_type)
  end subroutine ckkw_pseudo_shower_weights_init

  subroutine ckkw_pseudo_shower_weights_write (weights, unit)
    type(ckkw_pseudo_shower_weights_t), intent(in) :: weights
    integer, intent(in), optional :: unit
    integer :: s, i, u
    u = given_output_unit (unit); if (u < 0) return
    s = size (weights%weights)
    write (u, "(1x,A)")  "CKKW (pseudo) shower weights: "
    do i = 1, s
       write (u, "(3x,I0,2(ES19.12))")  i, weights%weights(i), &
            weights%weights_by_type(i,:)
    end do
    write (u, "(3x,A,1x,I0)")  "alphaS =", weights%alphaS
  end subroutine ckkw_pseudo_shower_weights_write


end module ckkw_base
