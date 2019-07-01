! WHIZARD 2.2.8 Nov 22 2015
! 
! Copyright (C) 1999-2015 by 
!     Wolfgang Kilian <kilian@physik.uni-siegen.de>
!     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
!     Juergen Reuter <juergen.reuter@desy.de>
!     
!     with contributions from
!     Fabian Bach <fabian.bach@t-online.de>
!     Bijan Chokoufe <bijan.chokoufe@desy.de>
!     Christian Speckner <cnspeckn@googlemail.com> 
!     Soyoung Shim <soyoung.shim@desy.de>
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

module state_matrices_ut
  use unit_tests
  use state_matrices_uti
  
  implicit none
  private

  public :: state_matrix_test

contains
  
  subroutine state_matrix_test (u, results)
    integer, intent(in) :: u
    type(test_results_t), intent(inout) :: results
    call test (state_matrix_1, "state_matrix_1", &
         "check merge of quantum states of equal depth", &
         u, results)
    call test (state_matrix_2, "state_matrix_2", &
         "check factorizing 3-particle state matrix", &
         u, results)
    call test (state_matrix_3, "state_matrix_3", &
         "check factorizing 3-particle state matrix", &
         u, results)
    call test (state_matrix_4, "state_matrix_4", &
         "check raw I/O", &
         u, results)
    call test (state_matrix_5, "state_matrix_5", &
         "check flavor content", &
         u, results)  
  end subroutine state_matrix_test 
  

end module state_matrices_ut
