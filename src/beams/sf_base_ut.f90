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

module sf_base_ut
  use unit_tests
  use sf_base_uti
  
  implicit none
  private

  public :: sf_test_data_t

  public :: sf_base_test

contains
  
  subroutine sf_base_test (u, results)
    integer, intent(in) :: u
    type(test_results_t), intent(inout) :: results
    call test (sf_base_1, "sf_base_1", &
         "structure function configuration", &
         u, results)
    call test (sf_base_2, "sf_base_2", &
         "structure function instance", &
         u, results)
    call test (sf_base_3, "sf_base_3", &
         "alternatives for collinear kinematics", &
         u, results)
    call test (sf_base_4, "sf_base_4", &
         "alternatives for non-collinear kinematics", &
         u, results)
    call test (sf_base_5, "sf_base_5", &
         "pair spectrum with radiation", &
         u, results)
    call test (sf_base_6, "sf_base_6", &
         "pair spectrum without radiation", &
         u, results)
    call test (sf_base_7, "sf_base_7", &
         "direct access", &
         u, results)
    call test (sf_base_8, "sf_base_8", &
         "structure function chain configuration", &
         u, results)
    call test (sf_base_9, "sf_base_9", &
         "structure function chain instance", &
         u, results)
   call test (sf_base_10, "sf_base_10", &
        "structure function chain mapping", &
        u, results)
    call test (sf_base_11, "sf_base_11", &
         "structure function chain evaluation", &
         u, results)
    call test (sf_base_12, "sf_base_12", &
         "multi-channel structure function chain", &
         u, results)
    call test (sf_base_13, "sf_base_13", &
         "pair spectrum generator", &
         u, results)
    call test (sf_base_14, "sf_base_14", &
         "structure function generator evaluation", &
         u, results)
  end subroutine sf_base_test
  

end module sf_base_ut
