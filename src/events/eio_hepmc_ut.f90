! WHIZARD 2.7.1 Mar 27 2019
!
! Copyright (C) 1999-2019 by
!     Wolfgang Kilian <kilian@physik.uni-siegen.de>
!     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
!     Juergen Reuter <juergen.reuter@desy.de>
!
!     with contributions from
!     cf. main AUTHORS file
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

module eio_hepmc_ut
  use unit_tests
  use eio_hepmc_uti

  implicit none
  private

  public :: eio_hepmc_test

contains

  subroutine eio_hepmc_test (u, results)
    integer, intent(in) :: u
    type(test_results_t), intent(inout) :: results
    call test (eio_hepmc_1, "eio_hepmc_1", &
         "write event contents", &
         u, results)
    call test (eio_hepmc_2, "eio_hepmc_2", &
         "read event contents", &
         u, results)
  end subroutine eio_hepmc_test


end module eio_hepmc_ut
