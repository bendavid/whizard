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

module vegas_ut
  use unit_tests
  use vegas_uti

  implicit none
  private

  public :: vegas_test

contains
  subroutine vegas_test (u, results)
    integer, intent(in) :: u
    type(test_results_t), intent(inout) :: results
      call test (vegas_1, "vegas_1", "VEGAS initialisation and&
           & grid preparation", u, results)
      call test (vegas_2, "vegas_2", "VEGAS configuration and result object", u, results)
      call test (vegas_3, "vegas_3", "VEGAS integration of multi-dimensional gaussian", u, results)
      call test (vegas_4, "vegas_4", "VEGAS integration of three&
           &-dimensional factorisable polynomial function", u, results)
      call test (vegas_5, "vegas_5", "VEGAS integration and event&
           & generation of multi-dimensional gaussian", u, results)
      call test (vegas_6, "vegas_6", "VEGAS integrate and write grid, &
           & read grid and continue", u, results)
  end subroutine vegas_test

end module vegas_ut
