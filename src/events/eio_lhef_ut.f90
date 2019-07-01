! WHIZARD 2.4.0 Nov 28 2016
! 
! Copyright (C) 1999-2016 by 
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

module eio_lhef_ut
  use unit_tests
  use eio_lhef_uti
  
  implicit none
  private

  public :: eio_lhef_test

contains
  
  subroutine eio_lhef_test (u, results)
    integer, intent(in) :: u
    type(test_results_t), intent(inout) :: results
    call test (eio_lhef_1, "eio_lhef_1", &
         "write version 1.0", &
         u, results)
    call test (eio_lhef_2, "eio_lhef_2", &
         "write version 2.0", &
         u, results)
    call test (eio_lhef_3, "eio_lhef_3", &
         "write version 3.0", &
         u, results)
    call test (eio_lhef_4, "eio_lhef_4", &
         "read version 1.0", &
         u, results)
    call test (eio_lhef_5, "eio_lhef_5", &
         "read version 2.0", &
         u, results)
    call test (eio_lhef_6, "eio_lhef_6", &
         "read version 3.0", &
         u, results)
  end subroutine eio_lhef_test
  

end module eio_lhef_ut
