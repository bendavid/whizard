! WHIZARD 2.3.1 Aug 25 2016
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

module object_logical_ut

  use unit_tests
  use object_logical_uti
  
  implicit none
  private

  public :: object_logical_test

contains
  
  subroutine object_logical_test (u, results)
    integer, intent(in) :: u
    type(test_results_t), intent(inout) :: results
    call test (object_logical_1, "object_logical_1", &
         "values", &
         u, results)
    call test (object_logical_2, "object_logical_2", &
         "assignment", &
         u, results)
    call test (object_logical_3, "object_logical_3", &
         "composite assignment", &
         u, results)
    call test (object_logical_4, "object_logical_4", &
         "nontrivial assignment", &
         u, results)
    call test (object_logical_5, "object_logical_5", &
         "operator: not", &
         u, results)
    call test (object_logical_6, "object_logical_6", &
         "operator: and", &
         u, results)
    call test (object_logical_7, "object_logical_7", &
         "operator: or", &
         u, results)
    call test (object_logical_8, "object_logical_8", &
         "nested expressions", &
         u, results)
    call test (object_logical_9, "object_logical_9", &
         "build assignment from code", &
         u, results)
    call test (object_logical_10, "object_logical_10", &
         "build expressions from code", &
         u, results)  
  end subroutine object_logical_test
  

end module object_logical_ut
