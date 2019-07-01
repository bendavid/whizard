! WHIZARD 2.5.0 May 06 2017
!
! Copyright (C) 1999-2017 by
!     Wolfgang Kilian <kilian@physik.uni-siegen.de>
!     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
!     Juergen Reuter <juergen.reuter@desy.de>
!
!     with contributions from
!     Fabian Bach <fabian.bach@t-online.de>
!     Bijan Chokoufe <bijan.chokoufe@desy.de>
!     Christian Speckner <cnspeckn@googlemail.com>
!     So Young Shim <soyoung.shim@desy.de>
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

module vegas_uti
  use kinds, only: default
  use io_units
  use constants, only: pi
  use format_defs, only: FMT_10, FMT_12
  use rng_base
  use rng_stream
  use vegas

  implicit none
  private

  public :: vegas_1
  public :: vegas_2
  public :: vegas_3
  public :: vegas_4
  public :: vegas_5
  public :: vegas_6

  type, extends (vegas_func_t) :: vegas_test_func_t
     !
   contains
     procedure, public :: evaluate => vegas_test_func_evaluate
  end type vegas_test_func_t

  type, extends (vegas_func_t) :: vegas_gaussian_test_func_t
     !
   contains
     procedure, public :: evaluate => vegas_gaussian_evaluate
  end type vegas_gaussian_test_func_t

  type, extends (vegas_func_t) :: vegas_polynomial_func_t
     !
   contains
     procedure, public :: evaluate => vegas_polynomial_evaluate
   end type vegas_polynomial_func_t


contains
  real(default) function vegas_test_func_evaluate (self, x) result (f)
    class(vegas_test_func_t), intent(inout) :: self
    real(default), dimension(:), intent(in) :: x
    f = 1.0 / (pi**3)
    f = f / ( 1.0 - cos (x(1)) * cos (x(2)) * cos (x(3)))
  end function vegas_test_func_evaluate

  real(default) function vegas_gaussian_evaluate (self, x) result (f)
    class(vegas_gaussian_test_func_t), intent(inout) :: self
    real(default), dimension(:), intent(in) :: x
    real(default), parameter :: inv_sqrt_pi = 1._default / sqrt(pi)
    f = inv_sqrt_pi**size (x)
    f = f * exp (- dot_product(x, x))
  end function vegas_gaussian_evaluate

  real(default) function vegas_polynomial_evaluate (self, x) result (f)
    class(vegas_polynomial_func_t), intent(inout) :: self
    real(default), dimension(:), intent(in) :: x
    f = - 8. / 3. * (x(1) + 1.) * (x(2) - 1.) * x(3)
  end function vegas_polynomial_evaluate

  subroutine vegas_1 (u)
    integer, intent(in) :: u
    type(vegas_t) :: mc_integrator
    class(rng_t), allocatable :: rng
    class(vegas_func_t), allocatable :: func
    real(default), dimension(3), parameter :: x_lower = 0., &
         x_upper = pi
    real(default) :: result, abserr

    write (u, "(A)") "* Test output: vegas_1"
    write (u, "(A)") "*   Purpose: initialise the VEGAS MC integrator and the grid"
    write (u, "(A)")

    write (u, "(A)") "* Initialise random number generator (default seed)"
    write (u, "(A)")

    allocate (rng_stream_t :: rng)
    call rng%init ()

    call rng%write (u)

    write (u, "(A)")
    write (u, "(A)") "* Initialise MC integrator with n_dim = 3"
    write (u, "(A)")

    allocate (vegas_test_func_t :: func)
    mc_integrator = vegas_t (3)

    write (u, "(A)")
    write (u, "(A)") "* Initialise grid with n_calls = 10000"
    write (u, "(A)")

    call mc_integrator%set_limits (x_lower, x_upper)
    call mc_integrator%set_calls (10000)

    write (u, "(A)")
    write (u, "(A)") "* Integrate with n_it = 3 and n_calls = 10000 (Adaptation)"
    write (u, "(A)")

    call mc_integrator%integrate (func, rng, 3, result=result, abserr=abserr)
    write (u, "(2x,A," // FMT_12 // ",A," // FMT_12 // ")") "Result: ", result, " +/- ", abserr

    write (u, "(A)")
    write (u, "(A)") "* Integrate with n_it = 3 and n_calls = 2000 (Precision)"
    write (u, "(A)")

    call mc_integrator%set_calls (2000)
    call mc_integrator%integrate (func, rng, 3, result=result, abserr=abserr)
    write (u, "(2x,A," // FMT_12 // ",A," // FMT_12 // ")") "Result: ", result, " +/- ", abserr

    write (u, "(A)")
    write (u, "(A)") "* Cleanup"

    call mc_integrator%final ()
    call rng%final ()
    deallocate (rng)
  end subroutine vegas_1
  subroutine vegas_2 (u)
    integer, intent(in) :: u
    type(vegas_t) :: mc_integrator
    type(vegas_config_t) :: mc_integrator_config
    type(vegas_result_t) :: mc_integrator_result

    write (u, "(A)") "* Test output: vegas_2"
    write (u, "(A)") "*   Purpose: use transparent containers for&
         & configuration and result."
    write (u, "(A)")

    write (u, "(A)")
    write (u, "(A)") "* Initialise MC integrator with n_dim = 10"
    write (u, "(A)")

    mc_integrator = vegas_t (10)

    write (u, "(A)")
    write (u, "(A)") "* Initialise grid with n_calls = 10000 (Importance Sampling)"
    write (u, "(A)")

    call mc_integrator%set_calls (10000)

    write (u, "(A)")
    write (u, "(A)") "* Get VEGAS config object and write out"
    write (u, "(A)")

    call mc_integrator%get_config (mc_integrator_config)
    call mc_integrator_config%write (u)

    write (u, "(A)")
    write (u, "(A)") "* Get VEGAS empty result object and write out"
    write (u, "(A)")

    call mc_integrator%get_result (mc_integrator_result)
    call mc_integrator_result%write (u)

    write (u, "(A)")
    write (u, "(A)") "* Cleanup"

    call mc_integrator%final ()
  end subroutine vegas_2
  subroutine vegas_3 (u)
    integer, intent(in) :: u
    type(vegas_t) :: mc_integrator
    class(rng_t), allocatable :: rng
    class(vegas_func_t), allocatable :: func
    real(default), dimension(3), parameter :: x_lower_3 = -10._default, &
         x_upper_3 = 10._default
    type(vegas_config_t) :: mc_integrator_config
    type(vegas_grid_t) :: mc_integrator_grid
    type(vegas_result_t) :: mc_integrator_result

    real(default) :: result, abserr

    write (u, "(A)") "* Test output: vegas_3"
    write (u, "(A)") "*   Purpose: Integrate gaussian distribution."
    write (u, "(A)")

    allocate (rng_stream_t :: rng)
    call rng%init ()

    call rng%write (u)

    write (u, "(A)")
    write (u, "(A)") "* Initialise MC integrator with n_dim = 3"
    write (u, "(A)")

    allocate (vegas_gaussian_test_func_t :: func)
    mc_integrator = vegas_t (3)

    write (u, "(A)")
    write (u, "(A)") "* Initialise grid with n_calls = 10000"
    write (u, "(A)")

    call mc_integrator%set_limits (x_lower_3, x_upper_3)
    call mc_integrator%set_calls (10000)

    write (u, "(A)")
    write (u, "(A)") "* Get VEGAS config object and write out"
    write (u, "(A)")

    call mc_integrator%get_config (mc_integrator_config)
    call mc_integrator_config%write (u)

    write (u, "(A)")
    write (u, "(A)") "* Get VEGAS grid object and write out"
    write (u, "(A)")

    mc_integrator_grid = mc_integrator%get_grid ()
    call mc_integrator_grid%write (u)

    write (u, "(A)")
    write (u, "(A)") "* Integrate with n_it = 3 and n_calls = 20000 (Adaptation)"
    write (u, "(A)")

    call mc_integrator%integrate (func, rng, 3, result=result, abserr=abserr)
    write (u, "(2x,A," // FMT_12 // ",A," // FMT_12 // ")") "Result: ", result, " +/- ", abserr

    write (u, "(A)")
    write (u, "(A)") "* Integrate with n_it = 3 and n_calls = 2000 (Precision)"
    write (u, "(A)")

    call mc_integrator%set_calls (2000)
    call mc_integrator%get_config (mc_integrator_config)
    call mc_integrator_config%write (u)

    write (u, "(A)")

    call mc_integrator%integrate (func, rng, 3, result=result, abserr=abserr)
    write (u, "(2x,A," // FMT_12 // ",A," // FMT_12 // ")") "Result: ", result, " +/- ", abserr


    write (u, "(A)")
    write (u, "(A)") "* Get VEGAS result object and write out"
    write (u, "(A)")

    call mc_integrator%get_result (mc_integrator_result)
    call mc_integrator_result%write (u)

    write (u, "(A)")
    write (u, "(A)") "* Get VEGAS grid object and write out"
    write (u, "(A)")

    mc_integrator_grid = mc_integrator%get_grid ()
    call mc_integrator_grid%write (u)

    write (u, "(A)")
    write (u, "(A)") "* Cleanup"

    call mc_integrator%final ()
  end subroutine vegas_3
  subroutine vegas_4 (u)
    integer, intent(in) :: u
    type(vegas_t) :: mc_integrator
    class(rng_t), allocatable :: rng
    class(vegas_func_t), allocatable :: func
    real(default), dimension(3), parameter :: x_lower_3 = 0._default, &
         x_upper_3 = 1._default
    type(vegas_config_t) :: mc_integrator_config
    type(vegas_result_t) :: mc_integrator_result

    real(default) :: result, abserr

    write (u, "(A)") "* Test output: vegas_4"
    write (u, "(A)") "*   Purpose: Integrate gaussian distribution."
    write (u, "(A)")

    allocate (rng_stream_t :: rng)
    call rng%init ()

    call rng%write (u)

    write (u, "(A)")
    write (u, "(A)") "* Initialise MC integrator with n_dim = 3"
    write (u, "(A)")

    allocate (vegas_polynomial_func_t :: func)
    mc_integrator = vegas_t (3)

    write (u, "(A)")
    write (u, "(A)") "* Initialise grid with n_calls = 2000"
    write (u, "(A)")

    call mc_integrator%set_limits (x_lower_3, x_upper_3)
    call mc_integrator%set_calls (2000)

    write (u, "(A)")
    write (u, "(A)") "* Integrate with n_it = 3 and n_calls = 2000 (Adaptation)"
    write (u, "(A)")

    call mc_integrator%integrate (func, rng, 3, result=result, abserr=abserr)

    call mc_integrator%get_config (mc_integrator_config)
    call mc_integrator_config%write (u)

    write (u, "(A)")

    write (u, "(2x,A," // FMT_12 // ",A," // FMT_12 // ")") "Result: ", result, " +/- ", abserr

    write (u, "(A)")
    write (u, "(A)") "* Integrate with n_it = 3 and n_calls = 20000 (Precision)"
    write (u, "(A)")

    call mc_integrator%set_calls (20000)

    call mc_integrator%integrate (func, rng, 3, result=result, abserr=abserr)

    call mc_integrator%get_config (mc_integrator_config)
    call mc_integrator_config%write (u)

    write (u, "(A)")
    write (u, "(2x,A," // FMT_12 // ",A," // FMT_12 // ")") "Result: ", result, " +/- ", abserr

    write (u, "(A)")
    write (u, "(A)") "* Cleanup"

    call mc_integrator%final ()
  end subroutine vegas_4

  subroutine vegas_5 (u)
    integer, intent(in) :: u
    type(vegas_t) :: mc_integrator
    class(rng_t), allocatable :: rng
    class(vegas_func_t), allocatable :: func
    real(default), dimension(1), parameter :: x_lower_1 = -10._default, &
         x_upper_1 = 10._default
    type(vegas_config_t) :: mc_integrator_config
    type(vegas_result_t) :: mc_integrator_result

    integer :: i, u_event
    real(default), dimension(1) :: event
    real(default) :: result, abserr

    write (u, "(A)") "* Test output: vegas_5"
    write (u, "(A)") "*   Purpose: Integrate gaussian distribution."
    write (u, "(A)")

    allocate (rng_stream_t :: rng)
    call rng%init ()

    call rng%write (u)

    write (u, "(A)")
    write (u, "(A)") "* Initialise MC integrator with n_dim = 1"
    write (u, "(A)")

    allocate (vegas_gaussian_test_func_t :: func)
    mc_integrator = vegas_t (1)

    write (u, "(A)")
    write (u, "(A)") "* Initialise grid with n_calls = 20000"
    write (u, "(A)")

    call mc_integrator%set_limits (x_lower_1, x_upper_1)
    call mc_integrator%set_calls (20000)

    write (u, "(A)")
    write (u, "(A)") "* Integrate with n_it = 3 (Adaptation)"
    write (u, "(A)")

    call mc_integrator%integrate (func, rng, 3, opt_verbose=.true., result=result, abserr=abserr)
    call mc_integrator%get_config (mc_integrator_config)
    call mc_integrator_config%write (u)
    write (u, "(2x,A," // FMT_12 // ",A," // FMT_12 // ")") "Result: ", result, " +/- ", abserr

    write (u, "(A)")
    write (u, "(A)") "* Integrate with n_it = 3 and n_calls = 2000 (Precision)"
    write (u, "(A)")

    call mc_integrator%set_calls (2000)
    call mc_integrator%integrate (func, rng, 3, opt_verbose=.true., result=result, abserr=abserr)
    call mc_integrator%get_config (mc_integrator_config)
    call mc_integrator_config%write (u)
    write (u, "(2x,A," // FMT_12 // ",A," // FMT_12 // ")") "Result: ", result, " +/- ", abserr

    write (u, "(A)")
    write (u, "(A)") "* Generate 1000 events based on the adaptation."
    write (u, "(A)")

    do i = 1, 1000
       call mc_integrator%generate_event (func, rng, event)
       write (u, "(2x,I4,1x," // FMT_10 // ")") i, event
    end do

    write (u, "(A)")
    write (u, "(A)") "* Cleanup"

    call mc_integrator%final ()
  end subroutine vegas_5

  subroutine vegas_6 (u)
    integer, intent(in) :: u
    type(vegas_t) :: mc_integrator
    class(rng_t), allocatable :: rng
    class(vegas_func_t), allocatable :: func
    real(default), dimension(3), parameter :: x_lower_3 = 0._default, &
         x_upper_3 = 1._default
    type(vegas_config_t) :: mc_integrator_config
    type(vegas_result_t) :: mc_integrator_result

    real(default) :: result, abserr
    integer :: unit

    write (u, "(A)") "* Test output: vegas_6"
    write (u, "(A)") "*   Purpose: Write and read grid, and continue."
    write (u, "(A)")

    allocate (rng_stream_t :: rng)
    call rng%init ()

    call rng%write (u)

    write (u, "(A)")
    write (u, "(A)") "* Initialise MC integrator with n_dim = 3"
    write (u, "(A)")

    allocate (vegas_polynomial_func_t :: func)
    mc_integrator = vegas_t (3)

    write (u, "(A)")
    write (u, "(A)") "* Initialise grid with n_calls = 2000"
    write (u, "(A)")

    call mc_integrator%set_limits (x_lower_3, x_upper_3)
    call mc_integrator%set_calls (2000)

    write (u, "(A)")
    write (u, "(A)") "* Integrate with n_it = 3 and n_calls = 2000 (Adaptation)"
    write (u, "(A)")

    call mc_integrator%integrate (func, rng, 3, result=result, abserr=abserr)

    call mc_integrator%get_config (mc_integrator_config)
    call mc_integrator_config%write (u)

    write (u, "(A)")

    write (u,  "(2x,A," // FMT_12 // ",A," // FMT_12 // ")") "Result: ", result, " +/- ", abserr

    write (u, "(A)")
    write (u, "(A)") "* Write grid to file vegas_io.grid"
    write (u, "(A)")

    unit = free_unit ()
    open (unit, file = "vegas_io.grid", &
         action = "write", status = "replace")
    call mc_integrator%write_grid (unit)
    close (unit)

    write (u, "(A)")
    write (u, "(A)") "* Read grid from file vegas_io.grid"
    write (u, "(A)")

    call mc_integrator%final ()
    open (unit, file = "vegas_io.grid", &
         action = "read", status = "old")
    call mc_integrator%read_grid (unit)
    close (unit)

    write (u, "(A)")
    write (u, "(A)") "* Integrate with n_it = 3 and n_calls = 20000 (Precision)"
    write (u, "(A)")

    call mc_integrator%set_calls (20000)

    call mc_integrator%integrate (func, rng, 3, result=result, abserr=abserr)

    call mc_integrator%get_config (mc_integrator_config)
    call mc_integrator_config%write (u)

    write (u, "(A)")
    write (u,  "(2x,A," // FMT_12 // ",A," // FMT_12 // ")") "Result: ", result, " +/- ", abserr

    write (u, "(A)")
    write (u, "(A)") "* Cleanup"

    call mc_integrator%final ()
  end subroutine vegas_6

end module vegas_uti
