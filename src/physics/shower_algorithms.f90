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

module shower_algorithms

  use kinds, only: default
  use constants
  use unit_tests
  use rng_base

  implicit none
  private

  public :: shower_algorithms_test

  interface
    pure function XXX_function (x)
      import
      real(default) :: XXX_function
      real(default), dimension(:), intent(in) :: x
    end function XXX_function
  end interface
  interface
    pure function sudakov_p (x)
      import
      real(default) :: sudakov_p
      real(default), intent(in) :: x
    end function sudakov_p
  end interface

contains

  subroutine generate_vetoed (x, rng, overestimator, true_function, &
         sudakov, inverse_sudakov, scale_min)
    real(default), dimension(:), intent(out) :: x
    class(rng_t), intent(inout) :: rng
    procedure(XXX_function), pointer, intent(in) :: overestimator, true_function
    procedure(sudakov_p), pointer, intent(in) :: sudakov, inverse_sudakov
    real(default), intent(in) :: scale_min
    real(default) :: random, scale_max, scale
    scale_max = inverse_sudakov (one)
    do while (scale_max > scale_min)
       call rng%generate (random)
       scale = inverse_sudakov (random * sudakov (scale_max))
       call generate_on_hypersphere (x, overestimator, scale)
       call rng%generate (random)
       if (random < true_function (x) / overestimator (x)) then
          return !!! accept x
       end if
       scale_max = scale
    end do
  end subroutine generate_vetoed

  pure subroutine generate_on_hypersphere (x, overestimator, scale)
    real(default), dimension(:), intent(out) :: x
    procedure(XXX_function), pointer, intent(in) :: overestimator
    real(default), intent(in) :: scale
  end subroutine generate_on_hypersphere


  subroutine shower_algorithms_test (u, results)
    integer, intent(in) :: u
    type(test_results_t), intent(inout) :: results
    call test (shower_algorithms_1, "shower_algorithms_1", &
         "veto technique", &
         u, results)
  end subroutine shower_algorithms_test

  subroutine shower_algorithms_1 (u)
    integer, intent(in) :: u

    write (u, "(A)")  "* Test output: shower_algorithms_1"
    write (u, "(A)")  "*   Purpose: check veto technique"
    write (u, "(A)")

    write (u, "(A)")  "* Splitting functions:"
    write (u, "(A)")

    !call assert (u, vanishes (p_qqg_pol (z, +1, -1, +1)))
    !call assert (u, nearly_equal ( &
         !p_qqg_pol (z, +1, +1, -1) + p_qqg_pol (z, +1, +1, +1),
         !p_qqg (z))

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: shower_algorithms_1"

  end subroutine shower_algorithms_1


end module shower_algorithms
