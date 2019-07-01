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

module fks_regions_ut
  use unit_tests
  use fks_regions_uti

  implicit none
  private

  public :: fks_regions_test

contains

  subroutine fks_regions_test (u, results)
    integer, intent(in) :: u
    type(test_results_t), intent(inout) :: results
    call test(fks_regions_1, "fks_regions_1", &
         "Test flavor structure utilities", u, results)
    call test(fks_regions_2, "fks_regions_2", &
         "Test singular regions for final-state radiation for n = 2", &
         u, results)
    call test(fks_regions_3, "fks_regions_3", &
         "Test singular regions for final-state radiation for n = 3", &
         u, results)
    call test(fks_regions_4, "fks_regions_4", &
         "Test singular regions for final-state radiation for n = 4", &
         u, results)
    call test(fks_regions_5, "fks_regions_5", &
         "Test singular regions for final-state radiation for n = 5", &
         u, results)
    call test(fks_regions_6, "fks_regions_6", &
         "Test singular regions for initial-state radiation", &
         u, results)
    call test(fks_regions_7, "fks_regions_7", &
         "Check Latex output", u, results)
  end subroutine fks_regions_test


end module fks_regions_ut
