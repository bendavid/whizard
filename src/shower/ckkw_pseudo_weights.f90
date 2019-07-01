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

module ckkw_pseudo_weights

  use kinds, only: default
  use io_units
  use constants

  implicit none
  private

  public :: ckkw_pseudo_shower_weights_t
  public :: ckkw_pseudo_shower_weights_init
  public :: ckkw_pseudo_shower_weights_write

  type :: ckkw_pseudo_shower_weights_t
     real(default) :: alphaS
     real(default), dimension(:), allocatable :: weights
     real(default), dimension(:,:), allocatable :: weights_by_type
  end type ckkw_pseudo_shower_weights_t


contains

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


end module ckkw_pseudo_weights
