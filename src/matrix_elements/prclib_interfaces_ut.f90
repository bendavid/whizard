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

module prclib_interfaces_ut
  use kinds
  use system_dependencies, only: CC_IS_GNU, CC_HAS_QUADMATH
  use unit_tests
  use prclib_interfaces_uti

  implicit none
  private

  public :: prclib_interfaces_test

  public :: test_writer_4_t

contains

  subroutine prclib_interfaces_test (u, results)
    integer, intent(in) :: u
    type(test_results_t), intent(inout) :: results
    call test (prclib_interfaces_1, "prclib_interfaces_1", &
         "create driver object", &
         u, results)
    call test (prclib_interfaces_2, "prclib_interfaces_2", &
         "write driver file", &
         u, results)
    call test (prclib_interfaces_3, "prclib_interfaces_3", &
         "write makefile", &
         u, results)
    call test (prclib_interfaces_4, "prclib_interfaces_4", &
         "compile and link (Fortran module)", &
         u, results)
    call test (prclib_interfaces_5, "prclib_interfaces_5", &
         "compile and link (Fortran library)", &
         u, results)
    if (default == double .or. (CC_IS_GNU .and. CC_HAS_QUADMATH)) then
       call test (prclib_interfaces_6, "prclib_interfaces_6", &
            "compile and link (C library)", &
            u, results)
    end if
    call test (prclib_interfaces_7, "prclib_interfaces_7", &
         "cleanup", &
         u, results)
  end subroutine prclib_interfaces_test


end module prclib_interfaces_ut
