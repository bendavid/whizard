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

module eio_ascii_ut
  use unit_tests
  use eio_ascii_uti

  implicit none
  private

  public :: eio_ascii_test

contains

  subroutine eio_ascii_test (u, results)
    integer, intent(in) :: u
    type(test_results_t), intent(inout) :: results
    call test (eio_ascii_1, "eio_ascii_1", &
         "read and write event contents, format [ascii]", &
         u, results)
    call test (eio_ascii_2, "eio_ascii_2", &
         "read and write event contents, format [athena]", &
         u, results)
    call test (eio_ascii_3, "eio_ascii_3", &
         "read and write event contents, format [debug]", &
         u, results)
    call test (eio_ascii_4, "eio_ascii_4", &
         "read and write event contents, format [hepevt]", &
         u, results)
    call test (eio_ascii_5, "eio_ascii_5", &
         "read and write event contents, format [lha]", &
         u, results)
    call test (eio_ascii_6, "eio_ascii_6", &
         "read and write event contents, format [long]", &
         u, results)
    call test (eio_ascii_7, "eio_ascii_7", &
         "read and write event contents, format [mokka]", &
         u, results)
    call test (eio_ascii_8, "eio_ascii_8", &
         "read and write event contents, format [short]", &
         u, results)
    call test (eio_ascii_9, "eio_ascii_9", &
         "read and write event contents, format [lha_verb]", &
         u, results)
    call test (eio_ascii_10, "eio_ascii_10", &
         "read and write event contents, format [hepevt_verb]", &
         u, results)
  end subroutine eio_ascii_test


end module eio_ascii_ut
