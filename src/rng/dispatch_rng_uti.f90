! WHIZARD 2.4.1 Mar 24 2017
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

module dispatch_rng_uti

  use iso_varying_string, string_t => varying_string
  use variables
  use diagnostics
  use rng_base
  use dispatch_rng

  implicit none
  private

  public :: dispatch_rng_factory_test

  public :: dispatch_rng_1

contains

  subroutine dispatch_rng_1 (u)
    integer, intent(in) :: u
    type(var_list_t) :: var_list
    class(rng_factory_t), allocatable :: rng_factory

    write (u, "(A)")  "* Test output: dispatch_rng_1"
    write (u, "(A)")  "*   Purpose: select random-number generator"
    write (u, "(A)")

    call var_list%init_defaults (0)

    write (u, "(A)")  "* Allocate RNG factory as rng_test_factory_t"
    write (u, "(A)")

    call var_list%set_string (&
         var_str ("$rng_method"), &
         var_str ("unit_test"), is_known = .true.)
    call var_list%set_int (&
         var_str ("seed"), 1, is_known = .true.)
    call dispatch_rng_factory (rng_factory, var_list)
    call rng_factory%write (u)
    deallocate (rng_factory)

    write (u, "(A)")
    write (u, "(A)")  "* Allocate RNG factory as rng_tao_factory_t"
    write (u, "(A)")

    call var_list%set_string (&
         var_str ("$rng_method"), &
         var_str ("tao"), is_known = .true.)
    call dispatch_rng_factory (rng_factory, var_list)
    call rng_factory%write (u)
    deallocate (rng_factory)

    write (u, "(A)")
    write (u, "(A)")  "* Allocate RNG factory as rng_stream_factory_t"
    write (u, "(A)")

    call var_list%set_string (&
         var_str ("$rng_method"), &
         var_str ("rng_stream"), is_known = .true.)
    call dispatch_rng_factory (rng_factory, var_list)
    call rng_factory%write (u)
    deallocate (rng_factory)

    call var_list%final ()

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: dispatch_rng_1"

  end subroutine dispatch_rng_1


  subroutine dispatch_rng_factory_test (rng_factory, var_list_global, var_list_local)
    use rng_base
    use rng_base_ut, only: rng_test_factory_t
    class(rng_factory_t), allocatable, intent(inout) :: rng_factory
    type(var_list_t), intent(inout) :: var_list_global
    type(var_list_t), intent(in), optional :: var_list_local
    type(var_list_t) :: local
    type(string_t) :: rng_method
    if (present (var_list_local)) then
       local = var_list_local
    else
       local = var_list_global
    end if
    rng_method = &
         local%get_sval (var_str ("$rng_method"))
    select case (char (rng_method))
    case ("unit_test")
       allocate (rng_test_factory_t :: rng_factory)
       call msg_message ("RNG: Initializing Test random-number generator")
    end select
  end subroutine dispatch_rng_factory_test


end module dispatch_rng_uti
